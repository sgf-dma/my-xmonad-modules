{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}

module Sgf.XMonad.Restartable
    ( modifyXS
    , ProcessClass (..)
    , withProcess
    , getProcess
    , getProcesses
    , manageProcess
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
    , showKeys
    , ProgConfig (..)
    , addProg
    , launchProg
    , handleProgs
    , Arguments (..)
    , NoArgs (..)
    -- Default program.
    , Program
    -- Program lenses.
    , progBin
    , progArgs
    , progWait
    , progWorkspace
    , progLaunchKey
    , progStartup
    --, progPid
    , defaultProgram
    , progCmd
    -- Program's RestartClass methods implementation.
    , programRunP
    , programKillP
    , programManageP
    , programDoLaunch
    , programLaunchAtStartup
    , programLaunchKey
    , programModifyPATH
    )
  where

import Data.List
import Data.Maybe
import Data.Typeable
import Data.Monoid
import Control.Monad
import Control.Exception (try, IOException)
import Control.Concurrent (threadDelay)
import System.Posix.Process (getProcessPriority)
import System.Posix.Signals (signalProcess, sigTERM)
import System.Posix.Types (ProcessID)

import XMonad
import XMonad.Hooks.ManageHelpers
import qualified XMonad.Util.ExtensibleState as XS

import Sgf.Data.List
import Sgf.Control.Lens
import Sgf.XMonad.Util.Run
import Sgf.XMonad.Util.EZConfig


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

-- Run given ManageHook, if new process PID matches.
manageProcess :: ProcessClass a => ManageHook -> Maybe a -> MaybeManageHook
manageProcess mh mx = do
    mp <- pid
    if isJust mx && mp == viewA pidL (fromJust mx)
      then Just <$> mh
      else return Nothing

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
    launchKey :: a -> [(ButtonMask, KeySym)]
    launchKey       = const []
    modifyPATH :: a -> X (Maybe ([FilePath] -> [FilePath]))
    modifyPATH      = const $ return (Just id)

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

-- FIXME: Should i trace only one program or all of them?
-- Print all tracked in Extensible State programs with given type.
traceP :: RestartClass a => a -> X ()
traceP y            = getProcesses y >>= mapM_ (trace . show)

-- Show program's launch key.
showKeys :: RestartClass a => a -> String
showKeys x       = case launchKey x of
                    [] -> ""
                    ks -> "Keys " ++ unwords (map show ks)
                            ++ " launch " ++ show x

-- Store some records of XConfig modified for particular program.
data ProgConfig l   = ProgConfig
                        { progManageHook  :: MaybeManageHook
                        , progLogHook     :: X ()
                        , progStartupHook :: X ()
                        , progKeys        :: XConfig l
                                             -> [((ButtonMask, KeySym), X ())]
                        , showProgKeys    :: String
                        }

-- Create ProgConfig for RestartClass instance.
addProg :: RestartClass a => a -> ProgConfig l
addProg x           = ProgConfig
                        -- Create MaybeManageHook from program's ManageHook.
                        { progManageHook  = manageProg x
                        -- Programs does not write to log.
                        , progLogHook     = return ()
                        -- Execute doLaunchP at startup.
                        , progStartupHook = when (launchAtStartup x)
                                                 (doLaunchP x)
                        -- Add keys for executing doLaunchP .
                        , progKeys        = launchProg x
                        -- And show these keys.
                        , showProgKeys    = showKeys x
                        }

-- Add key executing doLaunchP action of program.
launchProg :: RestartClass a => a -> XConfig l -> [((ButtonMask, KeySym), X ())]
launchProg x (XConfig {modMask = m}) = do
    (mk, k) <- launchKey x
    return ((m .|. mk, k), doLaunchP x)

-- Create MaybeManageHook, which executes program's ManageHook only, if
-- current Window pid (from _NET_WM_PID) matches pid of any program with the
-- same type stored in Extensible State.
manageProg :: RestartClass a => a -> MaybeManageHook
manageProg y        = liftX (getProcess y) >>= manageProcess (manageP y)

-- Merge ProgConfig-s into existing XConfig properly and add key for showing
-- program launch keys.
handleProgs :: Maybe (ButtonMask, KeySym)
               -> [ProgConfig l] -> XConfig l -> XConfig l
handleProgs mt ps cf = addProgKeys . addShowKey $ cf
    -- Run only one, matched program's ManageHook for any Window.  Program
    -- ManageHook-s may use `pid` function, which requests _NET_WM_PID window
    -- property, and sometimes it returns Nothing even though process has
    -- started and Extensible State contains correct pid. Probably, i should
    -- wait for a bit.
    { manageHook    = do
                        liftIO $ threadDelay 300000
                        composeOne (map progManageHook ps) <+> manageHook cf
    -- Restart all programs at xmonad startup.
    -- Note, the order in startupHook is important: `ewmh` function from
    -- XMonad.Hooks.EwmhDesktops also sets some atoms in _NET_SUPPORTED and
    -- uses 'propModeReplace'. Thus, it should be applied (if ever) *before*
    -- to not overwrite my changes.
    , startupHook   = do
                        mapM_ progStartupHook ps
                        startupHook cf
                        addWMPidSupport
    -- Log to all programs.
    , logHook       = mapM_ progLogHook ps >> logHook cf
    }
  where
    -- Merge program keys appending actions (not overwriting) for duplicate
    -- keys. Thus, if several programs use the same key, it'll launch them
    -- all. Then add resulting key list to xmonad keys (overwriting matches
    -- now).
    --addProgKeys :: XConfig l1 -> XConfig l1
    addProgKeys = appEndo $ foldMap (Endo . (appendKeys <*>) . progKeys) ps
    -- Key for showing program launch keys.
    addShowKey :: XConfig l -> XConfig l
    addShowKey  = additionalKeys' <*> mt `maybeKey` spawn' "xmessage"
                    [ "-default", "okay"
                    , unlines . filter (/= "") . map showProgKeys $ ps
                    ]
    addWMPidSupport :: X ()
    addWMPidSupport = withDisplay $ \dpy -> do
        r <- asks theRoot
        a <- getAtom "_NET_SUPPORTED"
        c <- getAtom "ATOM"
        supp <- getAtom "_NET_WM_PID"
        io $ changeProperty32 dpy r a c propModeAppend [fromIntegral supp]

class Arguments a where
    serialize   :: MonadIO m => a -> m [String]
    defaultArgs :: a
data NoArgs         = NoArgs
  deriving (Show, Read, Typeable, Eq)
instance Arguments NoArgs where
    serialize _     = return []
    defaultArgs     = NoArgs

-- Default program providing set of fields needed for regular program and
-- default runP implementation.  Note: when using newtypes around Program
-- never define `doLaunchP (p x) = doLaunchP x`, because in that case value of
-- type Program will be added to Extensible State,   but *not* value of type
-- (p Program) (i.e. newtype). Instead, i should define `doLaunchP` for
-- newtype p directly, e.g. `doLaunchP = restartP` (or not define at all and
-- use default).
data Program a where
    Program :: Arguments a =>
               { _progPid  :: Maybe ProcessID
               , _progBin  :: FilePath
               , _progArgs :: a
               , _progPATH :: Maybe [FilePath]
               , _progWait :: Int
               , _progWorkspace :: String   -- Simplified manageP .
               , _progLaunchKey :: [(ButtonMask, KeySym)]
               , _progStartup :: Bool
               } -> Program a
deriving instance Typeable Program
deriving instance Show a => Show (Program a)
deriving instance (Arguments a, Read a) => Read (Program a)

progPid :: LensA (Program a) (Maybe ProcessID)
progPid f z@(Program {_progPid = x})
                    = fmap (\x' -> z{_progPid = x'}) (f x)
progBin :: LensA (Program a) FilePath
progBin f z@(Program {_progBin = x})
                    = fmap (\x' -> z{_progBin = x'}) (f x)
progArgs :: LensA (Program a) a
progArgs f z@(Program {_progArgs = x})
                    = fmap (\x' -> z{_progArgs = x'}) (f x)
progPATH :: LensA (Program a) (Maybe [FilePath])
progPATH f z@(Program {_progPATH = x})
                    = fmap (\x' -> z{_progPATH = x'}) (f x)
-- Wait specified number of microseconds after spawning a program. Only
-- positive integers (or zero) allowed in progWait .
progWait :: LensA (Program a) Int
progWait f z@(Program {_progWait = x})
                    = fmap (\x' -> z{_progWait = unsignedInt x'}) (f x)
  where
    unsignedInt :: Int -> Int
    unsignedInt i
      | i < 0       = 0
      | otherwise   = i
progWorkspace :: LensA (Program a) String
progWorkspace f z@(Program {_progWorkspace = x})
                    = fmap (\x' -> z{_progWorkspace = x'}) (f x)
progLaunchKey :: LensA (Program a) [(ButtonMask, KeySym)]
progLaunchKey f z@(Program {_progLaunchKey = x})
                    = fmap (\x' -> z{_progLaunchKey = x'}) (f x)
progStartup :: LensA (Program a) Bool
progStartup f z@(Program {_progStartup = x})
                    = fmap (\x' -> z{_progStartup = x'}) (f x)
defaultProgram :: Arguments a => Program a
defaultProgram      = Program
                        { _progPid  = Nothing
                        , _progBin  = ""
                        , _progArgs = defaultArgs
                        , _progPATH = Nothing
                        , _progWait = 0
                        , _progWorkspace = ""
                        , _progLaunchKey = []
                        , _progStartup = True
                        }

-- Just a helper function.
progCmd :: (MonadIO m, Arguments a) => Program a -> m (FilePath, [String])
progCmd x           = do
                        args <- serialize (viewA progArgs x)
                        return (viewA progBin x, args)

-- I make Program's RestartClass methods available as separate functions,
-- because when definining new RestartClass instances for type based on
-- Program (particularly, for which i can define `Lens a (Program b)`), i may
-- want to use some methods as they were defined for Program. Though, i can
-- just call corresponding method on Program value created from type a value,
-- this may not work as expected: if Program's method calls other RestartClass
-- methods (e.g. `runP` below calls `modifyPATH`), then, when invoked on
-- Program value, that other method would be choosed from Program's instance,
-- but not from type a instance as i may expect (e.g. `modifyPATH` of
-- Program's instance would be used, but not `modifyPATH` from type a
-- instance).
programRunP :: (RestartClass a, Arguments b) => LensA a (Program b)
               -> a -> X a
programRunP progL x = do
                        let w = viewA (progL . progWait) x
                        f <- modifyPATH x
                        p <- progCmd (viewA progL x) >>=
                             uncurry (spawnPIDWithPATH' f)
                        when (w > 0) $ io (threadDelay w)
                        return (setA pidL (Just p) x)
programKillP :: (Arguments b, Typeable b, Show b, Read b, Eq b)
                => LensA a (Program b) -> a -> X a
programKillP progL  = modifyAA progL killP
programManageP :: Arguments b => LensA a (Program b) -> a -> ManageHook
programManageP progL x  = let w = viewA (progL . progWorkspace) x
                          in  if null w then idHook else doShift w
programDoLaunch :: (Arguments b, Typeable b, Show b, Read b, Eq b)
                   => LensA a (Program b) -> a -> X ()
programDoLaunch progL   = doLaunchP . viewA progL
programLaunchAtStartup :: Arguments b => LensA a (Program b) -> a -> Bool
programLaunchAtStartup progL    = viewA (progL . progStartup)
programLaunchKey :: Arguments b => LensA a (Program b) -> a
                    -> [(ButtonMask, KeySym)]
programLaunchKey progL  = viewA (progL . progLaunchKey)
programModifyPATH :: Arguments b => LensA a (Program b) -> a
                     -> X (Maybe ([FilePath] -> [FilePath]))
programModifyPATH progL = return . Just .  maybe id const
                            . viewA (progL . progPATH)

-- I assume only one instance of each program by default. I.e. different
-- programs should have different types.
instance Eq a => Eq (Program a) where
    x == y          =      viewA progBin  x == viewA progBin  y
                        && viewA progArgs x == viewA progArgs y
instance Arguments a => Monoid (Program a) where
    x `mappend` y   = setA progPid (viewA progPid x) y
    mempty          = defaultProgram
instance (Arguments a, Typeable a, Show a, Read a, Eq a)
         => ProcessClass (Program a) where
    pidL            = progPid
instance (Arguments a, Typeable a, Show a, Read a, Eq a)
         => RestartClass (Program a) where
    runP x          = programRunP id x
    launchAtStartup = programLaunchAtStartup id
    launchKey       = programLaunchKey id
    manageP         = programManageP id
    -- I interpret Nothing in `progPATH` as "use default" (here it means "use
    -- PATH from environment"). Thus, it's not possible to disable PATH search
    -- using `progPATH`: i should either define `modifyPATH` to `const
    -- Nothing` (note, there `Nothing` has different meaning, which is defined
    -- by `searchPATH` function) or use slashes in `progBin`.
    modifyPATH x    = return . Just $ maybe id const (viewA progPATH x)

