{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Docks.Xmobar
    ( Xmobar
    -- I don't export this Lens, because it will allow to construct Xmobar
    -- value with broken (xmobarConf -> progArgs) relationship and then
    -- xmobarConf Lens will break Lens laws (see below).
    -- , xmobarProg
    , xmobarConf
    , xmobarPP
    , xmobarToggle
    , xmobarLaunch
    , defaultXmobar
    , defaultXmobarPP
    )
  where

import Data.Monoid
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
import Sgf.XMonad.Util.Run (spawnPipe')
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

-- This Xmobar definition suitable for launching several xmobars. They will
-- be distinguished by config file name.
data Xmobar      = Xmobar
                        { _xmobarProg    :: Program
                        , _xmobarConf    :: FilePath
                        , _xmobarPP      :: Maybe L.PP
                        , _xmobarToggle  :: Maybe (ButtonMask, KeySym)
                        , _xmobarLaunch  :: [(ButtonMask, KeySym)]
                        }
  deriving (Typeable)
-- This Lens exposes underlying Program fields. Using it directly may break
-- some relationships defined by other Lenses (like xmobarConf) and will cause
-- them to break Lens laws. Thus, i should use it with care.
xmobarProg :: LensA Xmobar Program
xmobarProg f z@(Xmobar {_xmobarProg = x})
                    = fmap (\x' -> z{_xmobarProg = x'}) (f x)
-- This is Lens to xmobar command-line argument, so it calls writeProgArgs
-- function after updating Xmobar record for writing current Xmobar state to
-- progArgs.  Note, that such Lens definition breaks law: 'set l (get l a) a =
-- a', when applied to wrong Xmobar value (e.g. if a has non-null xmobarConf
-- and null progArgs; such value can be created by first applying xmobarConf
-- lens and then manually applying (xmobarProg .  progArgs) lens).  Though,
-- with current Eq definition, this law holds even in that case. For ensuring,
-- that law doesn't break, i should always overwrite default Xmobar values and
-- do not export xmobarProg lens.
xmobarConf :: LensA Xmobar FilePath
xmobarConf f z@(Xmobar {_xmobarConf = x})
                    = fmap (\x' -> writeProgArgs z{_xmobarConf = x'}) (f x)
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
xmobarPP f z@(Xmobar {_xmobarPP = x})
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
xmobarPP' f z@(Xmobar {_xmobarPP = x})
                    = fmap (\x' -> z{_xmobarPP = x'}) (f x)
xmobarToggle :: LensA Xmobar (Maybe (ButtonMask, KeySym))
xmobarToggle f z@(Xmobar {_xmobarToggle = x})
                    = fmap (\x' -> z{_xmobarToggle = x'}) (f x)
xmobarLaunch :: LensA Xmobar [(ButtonMask, KeySym)]
xmobarLaunch f z@(Xmobar {_xmobarLaunch = x})
                    = fmap (\x' -> z{_xmobarLaunch = x'}) (f x)
-- Default for type, but not default expected by users of this type.
defaultXmobar' :: Xmobar
defaultXmobar'      = Xmobar
                        { _xmobarProg   = defaultProgram
                        , _xmobarConf   = ""
                        , _xmobarPP     = Nothing
                        , _xmobarToggle = Nothing
                        , _xmobarLaunch = []
                        }
-- Create progArgs from Xmobar records representing xmobar command-line
-- arguments. This function should be called from all Lenses to such records.
-- This is one-way conversion, i.e. old value of progArgs will be overwritten.
-- So, if i want to provide a way for user for adding unknown options to
-- progArgs, i should create special Xmobar record for that and merge its
-- value here.
writeProgArgs :: Xmobar -> Xmobar
writeProgArgs x     = let xcf = viewA xmobarConf x
                      in  setA (xmobarProg . progArgs) [xcf] x

-- Default expected by user's of Xmobar type. Usually it should be used
-- instead of type default. Particularly, this ensures, that all Lens laws
-- hold (see xmobarConf Lens).  All Xmobar values should be created by
-- overwriting default (this or type default) through Lenses (PP lenses
-- provided by XMonad.Docks). Here i use Lenses over type default for ensuring
-- correct xmobarConf initialization.
-- Also, note, that i should not initialize PP in defaultXmobar, otherwise
-- runP will open pipe to xmobar and xmonad blocks, when pipe fills up. Thus,
-- if user uses StdinReader in xmobarrc, he should set PP explicitly (by
-- overwriting defaultXmobarPP).
defaultXmobar :: Xmobar
defaultXmobar       = setA (xmobarProg . progBin) "xmobar"
                        . setA xmobarConf ".xmobarrc"
                        $ defaultXmobar'

-- Show and Read instances omiting some non-showable/non-readable records.
instance Show Xmobar where
    showsPrec d x   = showParen (d > app_prec) $
        showString "Xmobar {_xmobarProg = " . showsPrec d (viewA xmobarProg x)
        . showString ", _xmobarConf = "     . showsPrec d (viewA xmobarConf x)
        . showString ", _xmobarToggle = "   . showsPrec d (viewA xmobarToggle x)
        . showString ", _xmobarLaunch = "   . showsPrec d (viewA xmobarLaunch x)
        . showString "}"
      where
        app_prec    = 10
instance Read Xmobar where
    readsPrec d     = readParen (d > app_prec) . runStateT $ do
        readLexsM ["Xmobar"]
        xp <- readLexsM ["{", "_xmobarProg", "="] >> readsPrecM d
        xc <- readLexsM [",", "_xmobarConf", "="] >> readsPrecM d
        xt <- readLexsM [",", "_xmobarToggle", "="] >> readsPrecM d
        xl <- readLexsM [",", "_xmobarLaunch", "="] >> readsPrecM d
        readLexsM ["}"]
        -- The same as above: i need to overwrite records of defaultXmobar
        -- here, so right after reading saved extensible state ppOutput will
        -- be set to ignore output, until xmobar process will be restarted.
        let x = setA xmobarProg xp
                  . setA xmobarConf xc
                  . setA xmobarToggle xt
                  . setA xmobarLaunch xl
                  $ defaultXmobar
        return x
      where
        app_prec    = 10

instance Eq Xmobar where
    (==)            = (==) `on` viewA xmobarConf
-- Just use second argument, but *merge* Program records. I need to mappend
-- Program records, because Program may contain some fields, which i don't
-- know how to merge or don't know about them at all.  But when some Xmobar
-- lenses modify Program record (e.g. xmobarConf), merging Program records may
-- lead to incorrect (for Xmobar type) resulting Program (and lens laws won't
-- hold anymore). Thus, to ensure, that result is correct, i call
-- `writeProgArgs` for overwriting "generated" (from Xmobar records) part of
-- new Program record.
instance Monoid Xmobar where
    x `mappend` y   = writeProgArgs
                        $ modifyA xmobarProg (viewA xmobarProg x `mappend`) y
    mempty          = defaultXmobar
instance ProcessClass Xmobar where
    pidL            = xmobarProg . pidL
instance RestartClass Xmobar where
    runP x          = userCodeDef x $ do
        xcf <- absXmobarConf
        liftIO $ doesFileExist' xcf `catch` (throw . XmobarConfException)
        case viewA xmobarPP' x of
          Just _    -> do
            (h, p) <- spawnPipe' "xmobar" (viewA (xmobarProg . progArgs) x)
            return
              . setA pidL (Just p)
              . setA (xmobarPP' . maybeL . ppOutputL) (hPutStrLn h)
              $ x
          Nothing   -> modifyAA xmobarProg runP x
      where
        -- If xmobarConf is relative, take it from home directory, not from
        -- current directory.
        absXmobarConf :: MonadIO m => m FilePath
        absXmobarConf   = liftIO $ do
          d <- getHomeDirectory
          let cf = viewA xmobarConf x
          return (if isRelative cf then d </> cf else cf)
    -- I need to reset pipe (to ignore output), because though process got
    -- killed, xmobar value still live in Extensible state and dockLog does
    -- not check process existence - just logs according to PP, if any.
    killP           = modifyAA xmobarProg killP
                        . modifyA (xmobarPP' . maybeL) resetPipe
    doLaunchP       = restartP
    launchKey       = viewA xmobarLaunch
instance DockClass Xmobar where
    dockToggleKey   = viewA xmobarToggle
    ppL             = xmobarPP

data XmobarException    = XmobarConfException FileException
  deriving (Typeable)
instance Show XmobarException where
    show (XmobarConfException x) = "Xmobar config: " ++ show x
instance Exception XmobarException where

