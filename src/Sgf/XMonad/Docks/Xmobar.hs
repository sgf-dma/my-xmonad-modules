{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Docks.Xmobar
    ( XmobarArgs
    , xmobarConf
    , Xmobar
    , xmobarProg
    , xmobarPP
    , xmobarToggle
    , defaultXmobar
    , defaultXmobarPP
    )
  where

import Data.Function (on)
import Control.Monad.State
import Control.Exception
import System.IO (hPutStrLn)
import System.FilePath
import System.Directory (getHomeDirectory)

import XMonad
import qualified XMonad.Hooks.DynamicLog as L

import Sgf.Data.List
import Sgf.Control.Lens
import Sgf.Control.Exception
import Sgf.XMonad.Util.Run (spawnPipeWithPATH')
import Sgf.XMonad.Restartable
import Sgf.XMonad.Docks

-- Ignore anything going to ppOutput.
resetPipe :: L.PP -> L.PP
resetPipe           = setA ppOutputL (const (return ()))

-- Redefine default xmobarPP to ignore all output. I need this to avoid
-- outputting status information (by dockLog and dynamicLogWithPP) to
-- ~/.xsession-errors (where xmonad's stdout is connected) until corresponding
-- xmobar process is started and RestartClass's Xmobar instance will
-- initialize ppOutput with pipe connected to that process's stdin.
defaultXmobarPP :: L.PP
defaultXmobarPP     = resetPipe L.xmobarPP

-- If only filename is specified (e.g. for xmobar conf), it's taken from home
-- directory. Otherwise, the path preserved as is. This adds default for most
-- common case (filename only) and still allows to specify arbitrary path.
normaliseConf :: MonadIO m => FilePath -> m FilePath
normaliseConf cf
  | cf /= [] && takeFileName cf == cf
                    = fmap (</> cf) (liftIO getHomeDirectory)
  | otherwise       = return cf

-- XmobarArgs should provide options *with* container. I use records as
-- container, because only in that case i may easily define Eq instance to
-- consider only certain records (options). Any list-like container will
-- inherently make option lists with different length non-equal.
data XmobarArgs     = XmobarArgs {_xmobarConf :: FilePath}
  deriving (Show, Read, Typeable, Eq)
xmobarConf :: LensA XmobarArgs FilePath
xmobarConf f z@XmobarArgs {_xmobarConf = x}
                    = fmap (\x' -> z{_xmobarConf = x'}) (f x)
instance Arguments XmobarArgs where
    defaultArgs     = XmobarArgs {_xmobarConf = ".xmobarrc"}
    serialize x     = do
                        xcf <- normaliseConf (viewA xmobarConf x)
                        unless' (null xcf) (return [xcf])

-- This Xmobar definition suitable for launching several xmobars. They will
-- be distinguished by config file name.
data Xmobar         = Xmobar
                        { _xmobarProg    :: Program XmobarArgs
                        , _xmobarPP      :: Maybe L.PP
                        , _xmobarToggle  :: Maybe (ButtonMask, KeySym)
                        }
  deriving (Typeable)
xmobarProg :: LensA Xmobar (Program XmobarArgs)
xmobarProg f z@Xmobar {_xmobarProg = x}
                    = fmap (\x' -> z{_xmobarProg = x'}) (f x)
-- Lens to PP, which overwrites ppOutput: generally, ppOutput is set in runP
-- and should write to pipe to xmobar, so i should not allow modifying it for
-- anyone. So (old _xmobarPP value is on the left of plus sign, new - on the
-- right, and after equal sign is value to which i rewrite):
-- Nothing + Nothing = Nothing
-- Nothing + Just    = Just with pp set by `resetPipe`
-- Just    + Nothing = Nothing; does this correct? But if i make this Just,
--                     then there will be no way of setting _xmobarPP to
--                     Nothing at xmonad restart (recompile + reload)..
-- Just    + Just    = Just with pp preserved from old value
xmobarPP :: LensA Xmobar (Maybe L.PP)
xmobarPP f z@Xmobar {_xmobarPP = x}
                    = fmap (\y -> z
                            { _xmobarPP = modifyA maybeL
                                                  (updatePPOutput x) y
                            }
                        ) (f x)
  where
    updatePPOutput :: Maybe L.PP -> L.PP -> L.PP
    updatePPOutput Nothing  = resetPipe
    updatePPOutput (Just t) = setA ppOutputL (viewA ppOutputL t)
-- Lens to PP, which does not modify PP. Do not export, for internal use only!
xmobarPP' :: LensA Xmobar (Maybe L.PP)
xmobarPP' f z@Xmobar {_xmobarPP = x}
                    = fmap (\x' -> z{_xmobarPP = x'}) (f x)
xmobarToggle :: LensA Xmobar (Maybe (ButtonMask, KeySym))
xmobarToggle f z@Xmobar {_xmobarToggle = x}
                    = fmap (\x' -> z{_xmobarToggle = x'}) (f x)
-- I should not initialize PP in defaultXmobar, otherwise runP will open pipe
-- to xmobar and xmonad blocks, when pipe fills up. Thus, if user uses
-- StdinReader in xmobarrc, he should set PP explicitly (by overwriting
-- defaultXmobarPP).
defaultXmobar :: Xmobar
defaultXmobar       = Xmobar
                        { _xmobarProg   = setA progBin "xmobar" defaultProgram
                        , _xmobarPP     = Nothing
                        , _xmobarToggle = Nothing
                        }

-- Show and Read instances omiting some non-showable/non-readable records.
instance Show Xmobar where
    showsPrec d x   = showParen (d > app_prec) $
        showString "Xmobar {_xmobarProg = " . showsPrec d (viewA xmobarProg x)
        . showString ", _xmobarToggle = "   . showsPrec d (viewA xmobarToggle x)
        . showString "}"
      where
        app_prec    = 10
instance Read Xmobar where
    readsPrec d     = readParen (d > app_prec) . runStateT $ do
        readLexsM ["Xmobar"]
        xp <- readLexsM ["{", "_xmobarProg", "="] >> readsPrecM d
        xt <- readLexsM [",", "_xmobarToggle", "="] >> readsPrecM d
        readLexsM ["}"]
        -- The same as above: i need to overwrite records of defaultXmobar
        -- here, so right after reading saved extensible state ppOutput will
        -- be set to ignore output, until xmobar process will be restarted.
        let x = setA xmobarProg xp
                  . setA xmobarToggle xt
                  $ defaultXmobar
        return x
      where
        app_prec    = 10

instance Eq Xmobar where
    (==)            = (==) `on` viewA xmobarProg
-- Just use second argument, but *merge* Program records. I need to mappend
-- Program records, because Program may contain some fields, which i don't
-- know how to merge or don't know about them at all.
instance Monoid Xmobar where
    x `mappend` y   = modifyA xmobarProg (viewA xmobarProg x `mappend`) y
    mempty          = defaultXmobar
instance ProcessClass Xmobar where
    pidL            = xmobarProg . pidL
instance RestartClass Xmobar where
    runP x          = userCodeDef x $ case viewA xmobarPP' x of
          Just _    -> do
            f <- modifyPATH x
            (h, p) <- progCmd (viewA xmobarProg x) >>=
                      uncurry (spawnPipeWithPATH' f)
            return
              . setA pidL (Just p)
              . setA (xmobarPP' . maybeL . ppOutputL) (hPutStrLn h)
              $ x
          Nothing   -> modifyAA xmobarProg runP x
    -- I need to reset pipe (to ignore output), because though process got
    -- killed, xmobar value still live in Extensible state and dockLog does
    -- not check process existence - just logs according to PP, if any.
    killP           = modifyAA xmobarProg killP
                        . modifyA (xmobarPP' . maybeL) resetPipe
    doLaunchP       = restartP
    launchKey       = launchKey . viewA xmobarProg
    modifyPATH _    = do
        h <- liftIO getHomeDirectory
        return (Just (++ [h ++ ".local/bin"]))
instance DockClass Xmobar where
    dockToggleKey   = viewA xmobarToggle
    ppL             = xmobarPP

data XmobarException    = XmobarConfException FileException
  deriving (Typeable)
instance Show XmobarException where
    show (XmobarConfException x) = "Xmobar config: " ++ show x
instance Exception XmobarException where

