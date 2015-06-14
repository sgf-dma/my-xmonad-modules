{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Restartable
    ( modifyXS
    , ProcessClass (..)
    , withProcess
    , getProcess
    , getProcesses
    , findWins
    , RestartClass (..)
    , withProcessP
    , startP
    , startP'
    , stopP
    , stopP'
    , restartP
    , restartP'
    , toggleP
    , toggleP'
    , traceP
    , ProgConfig (..)
    , addProg
    , launchProg
    , handleProgs
    , Program
    , progBin
    , progArgs
    , defaultProgram
    )
  where

import Data.List
import Data.Maybe (maybeToList)
import Data.Monoid
import Control.Applicative
import Control.Monad
import Control.Exception (try, IOException)
import Control.Concurrent (threadDelay)
import System.Posix.Process (getProcessPriority)
import System.Posix.Signals (signalProcess, sigTERM)
import System.Posix.Types (ProcessID)

import XMonad
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Util.ExtensibleState as XS

import Sgf.Data.List
import Sgf.Control.Lens
import Sgf.XMonad.Util.Run


-- To avoid orphan (ExtensionClass [a]) instance, i need newtype.
newtype ListP a     = ListP {_processList :: [a]}
  deriving (Show, Read, Typeable)
emptyListP :: ListP a
emptyListP          = ListP []
processList :: LensA (ListP a) [a]
processList f (ListP xs)    = fmap ListP (f xs)

instance (Show a, Read a, Typeable a) => ExtensionClass (ListP a) where
    initialValue    = emptyListP
    extensionType   = PersistentExtension

modifyXS :: ExtensionClass a => (a -> X a) -> X ()
modifyXS f          = XS.get >>= f >>= XS.put

-- Strictly, all ProcessClass requirments are not required to define its
-- instance. But ProcessClass has these requirments, because i need
-- withProcess to work on its instances.
class (Eq a, Show a, Read a, Typeable a) => ProcessClass a where
    pidL            :: LensA a (Maybe ProcessID)

-- Run function on processes stored in Extensible State equal to given one. If
-- there is no such processes, add given process there and run function on it.
withProcess :: ProcessClass a => (a -> X a) -> a -> X ()
withProcess f y     = modifyXS $ modifyAA processList $
                        mapWhenM (== y) f . insertUniq y

-- Find value (process) equal to given process in Extensible State.
getProcess :: ProcessClass a => a -> X (Maybe a)
getProcess y        = XS.gets (find (== y) . viewA processList)

-- Get all processes stored in Extensible State with the type of given
-- process.
getProcesses :: ProcessClass a => a -> X [a]
getProcesses y      = XS.gets (viewA processList `asTypeOf` const [y])

-- Find all windows, which have pid of given ProcessClass instance in
-- _NET_WM_PID property.
findWins :: ProcessClass a => a -> X [Window]
findWins x          = withDisplay $ \dpy -> do
    rootw <- asks theRoot
    (_, _, wins) <- io $ queryTree dpy rootw
    -- If process is not running (and pid is Nothing in pidL), i should
    -- not search for windows, because `pid` also may return Nothing for
    -- windows, which does not set _NET_WM_PID property. Thus, it'll
    -- appear, that not running process will still have windows.
    flip (maybe (return [])) (viewA pidL x) $ \px ->
      filterM (\w -> maybe False (== px) <$> runQuery pid w) wins

class (Monoid a, ProcessClass a) => RestartClass a where
    -- Run a program.
    runP  :: a -> X a
    -- Terminate a program.  restartP' relies on Pid 'Nothing' after killP,
    -- because it then calls startP' and it won't do anything, if PID will
    -- still exist. So, in killP i should either set Pid to Nothing, or wait
    -- until it really terminates (defaultKillP does first).
    killP :: a -> X a
    killP           = modifyAA pidL $ \mp -> do
                        whenJust mp (liftIO . signalProcess sigTERM)
                        return Nothing
    -- ManageHook for this program.
    manageP :: a -> ManageHook 
    manageP         = const idHook
    -- How to start a program from startupHook or by key. Usually, if i use
    -- restartP here, program will be terminated and started again at xmonad
    -- restarts, but if i use startP here, program will only be restarted, if
    -- it wasn't running at xmonad restart.
    doLaunchP :: a -> X ()
    doLaunchP       = startP
    -- Whether to start program from startupHook ?
    launchAtStartup  :: a -> Bool
    launchAtStartup = const True
    -- Key for restarting program.
    launchKey  :: a -> Maybe (ButtonMask, KeySym)
    launchKey       = const Nothing

-- Version of withProcess, which `mappend`-s process we're searching by and
-- process we've found. Thus, some fields of found process may be updated.
-- It's important, that `mappend` runs before calling function f and with
-- found process first. I need this function, for updating changed (by user)
-- process records (e.g. progArgs) after xmonad restart (recompile and
-- reload), because `withProcess` will just use old process value stored in
-- Extensible State.
withProcessP :: RestartClass a => (a -> X a) -> a -> X ()
withProcessP f y    = withProcess (f . flip mappend y) y

-- Based on doesPidProgRun by Thomas Bach
-- (https://github.com/fuzzy-id/my-xmonad) .
refreshPid :: (MonadIO m, ProcessClass a) => a -> m a
refreshPid x        = case viewA pidL x of
    Nothing -> return x
    Just p  -> liftIO $
      either (const (setA pidL Nothing x)) (const x)
      `fmap` (try $ getProcessPriority p :: IO (Either IOException Int))

-- Here are versions of start/stop working on argument, not extensible state.
-- Run, if program is not running or already dead, otherwise do nothing.
startP' :: RestartClass a => a -> X a
startP' x           = do
  x' <- refreshPid x
  case viewA pidL x' of
    Nothing   -> runP x'
    Just _    -> return x'

-- Stop program.
stopP' :: RestartClass a => a -> X a
stopP'              = killP <=< refreshPid

-- Stop program and run again. Note, that it will run again only, if killP
-- kills it properly: either sets pid to Nothing or waits until it dies,
-- because startP' checks whether program is running.
restartP' :: RestartClass a => a -> X a
restartP'           = startP' <=< stopP'

-- Start program, if it does not run, and stop, if it is running.
toggleP' :: RestartClass a => a -> X a
toggleP' x          = do
  x' <- refreshPid x
  case viewA pidL x' of
    Nothing   -> runP x'
    Just _    -> killP x'

-- Here are versions of start/stop working on extensible state.  Usually,
-- these should be used.
startP :: RestartClass a => a -> X ()
startP              = withProcessP startP'

stopP :: RestartClass a => a -> X ()
stopP               = withProcessP stopP'

restartP :: RestartClass a => a -> X ()
restartP            = withProcessP restartP'

toggleP :: RestartClass a => a -> X ()
toggleP             = withProcessP toggleP'

-- Print all tracked in Extensible State programs with given type.
traceP :: RestartClass a => a -> X ()
traceP y            = getProcesses y >>= mapM_ (trace . show)


-- Store some records of XConfig modified for particular program.
data ProgConfig l   = ProgConfig
                        { progManageHook  :: MaybeManageHook
                        , progLogHook     :: X ()
                        , progStartupHook :: X ()
                        , progKeys        :: XConfig l
                                             -> [((ButtonMask, KeySym), X ())]
                        }

-- Create ProgConfig for RestartClass instance.
addProg :: (RestartClass a, LayoutClass l Window) => a -> ProgConfig l
addProg x           = ProgConfig
                        -- Create MaybeManageHook from program's ManageHook.
                        { progManageHook  = manageProg x
                        -- Programs does not write to log.
                        , progLogHook     = return ()
                        -- Execute doLaunchP at startup.
                        , progStartupHook = when (launchAtStartup x)
                                                 (doLaunchP x)
                        -- And add key for executing doLaunchP .
                        , progKeys        = launchProg x
                        }

-- Add key executing doLaunchP action of program.
launchProg :: RestartClass a => a -> XConfig l -> [((ButtonMask, KeySym), X ())]
launchProg x (XConfig {modMask = m}) = maybeToList $ do
    (mk, k) <- launchKey x
    return ((m .|. mk, k), doLaunchP x)

-- Create MaybeManageHook, which executes program's ManageHook only, if
-- current Window pid (from _NET_WM_PID) matches pid of any program with the
-- same type stored in Extensible State.
manageProg :: RestartClass a => a -> MaybeManageHook
manageProg y        = do
    mp <- pid
    mx <- liftX $ getProcess y
    if mp == maybe Nothing (viewA pidL) mx
      then Just <$> manageP y
      else return Nothing

-- Merge ProgConfig-s into existing XConfig properly.
handleProgs :: LayoutClass l Window => [ProgConfig l] -> XConfig l -> XConfig l
handleProgs ps cf   = addProgKeys $ cf
    -- Run only one, matched program's ManageHook for any Window.  Program
    -- ManageHook-s may use `pid` function, which requests _NET_WM_PID window
    -- property, and sometimes it returns Nothing even though process has
    -- started and Extensible State contains correct pid. Probably, i should
    -- wait for a bit.
    { manageHook    = do
                        liftIO $ threadDelay 300000
                        composeOne (map progManageHook ps) <+> manageHook cf
    -- Restart all programs at xmonad startup.
    , startupHook   = mapM_ progStartupHook ps >> startupHook cf
    -- Log to all programs.
    , logHook       = mapM_ progLogHook ps >> logHook cf
    }
  where
    -- Join keys for launching programs.
    --addProgKeys :: XConfig l1 -> XConfig l1
    addProgKeys     = additionalKeys <*> (concat <$> mapM progKeys ps)

-- Default program providing set of fields needed for regular program and
-- default runP implementation.  Note: when using newtypes around Program
-- never define `doLaunchP (p x) = doLaunchP x`, because in that case value of
-- type Program will be added to Extensible State,   but *not* value of type
-- (p Program) (i.e. newtype). Instead, i should define `doLaunchP` for
-- newtype p directly, e.g. `doLaunchP = restartP` (or not define at all and
-- use default).
data Program        = Program
                        { _progPid  :: Maybe ProcessID
                        , _progBin  :: FilePath
                        , _progArgs :: [String]
                        }
  deriving (Show, Read, Typeable)
progPid :: LensA Program (Maybe ProcessID)
progPid f z@(Program {_progPid = x})
                    = fmap (\x' -> z{_progPid = x'}) (f x)
progBin :: LensA Program FilePath
progBin f z@(Program {_progBin = x})
                    = fmap (\x' -> z{_progBin = x'}) (f x)
progArgs :: LensA Program [String]
progArgs f z@(Program {_progArgs = x})
                    = fmap (\x' -> z{_progArgs = x'}) (f x)
defaultProgram :: Program
defaultProgram      = Program
                        { _progPid = Nothing
                        , _progBin = ""
                        , _progArgs = []
                        }

-- I assume only one instance of each program by default. I.e. different
-- programs should have different types.
instance Eq Program where
    _ == _          = True
instance Monoid Program where
    x `mappend` y   = setA progBin (viewA progBin y)
                        . setA progArgs (viewA progArgs y)
                        $ x
    mempty          = defaultProgram
instance ProcessClass Program where
    pidL            = progPid
instance RestartClass Program where
    runP x          = do
                        p <- spawnPID' (viewA progBin x) (viewA progArgs x)
                        return (setA pidL (Just p) x)

