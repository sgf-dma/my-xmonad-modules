{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Restartable
    ( ListP
    , emptyListP
    , processList
    , modifyXS
    , ProcessClass (..)
    , withProcess
    , RestartClass (..)
    , startP
    , startP'
    , stopP
    , stopP'
    , restartP
    , restartP'
    , Program
    , progPid
    , progBin
    , progArgs
    , defaultProgram
    )
  where

import Control.Monad
import Control.Monad.Trans
import Control.Exception (try, IOException)
import System.Posix.Process (getProcessPriority)
import System.Posix.Signals (signalProcess, sigTERM)
import System.Posix.Types (ProcessID)

import XMonad.Core
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

class ProcessClass a => RestartClass a where
    runP  :: a -> X a
    -- restartP' relies on Pid 'Nothing' after killP, because it then calls
    -- startP' and it won't do anything, if PID will still exist. So, in killP
    -- i should either set Pid to Nothing, or wait until it really terminates
    -- (defaultKillP does first).
    killP :: a -> X a
    killP           = modifyAA pidL $ \mp -> do
                        whenJust mp (liftIO . signalProcess sigTERM)
                        return Nothing

-- Based on doesPidProgRun by Thomas Bach
-- (https://github.com/fuzzy-id/my-xmonad) .
refreshPid :: (MonadIO m, ProcessClass a) => a -> m a
refreshPid x        = case (viewA pidL x) of
    Nothing -> return x
    Just p  -> liftIO $ do
      either (const (setA pidL Nothing x)) (const x)
      `fmap` (try $ getProcessPriority p :: IO (Either IOException Int))

-- Here are versions of start/stop working on argument, not extensible state.
-- Run, if program is not running or already dead, otherwise do nothing.
startP' :: RestartClass a => a -> X a
startP' x           = do
  x' <- refreshPid x
  case (viewA pidL x') of
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

-- Here are versions of start/stop working on extensible state.  Usually,
-- these should be used.
startP :: RestartClass a => a -> X ()
startP              = withProcess startP'

stopP :: RestartClass a => a -> X ()
stopP               = withProcess stopP'

restartP :: RestartClass a => a -> X ()
restartP            = withProcess restartP'


-- Default program (for use in newtype-s).
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
instance ProcessClass Program where
    pidL            = progPid
instance RestartClass Program where
    runP x          = do
                        p <- spawnPID' (viewA progBin x) (viewA progArgs x)
                        return (setA pidL (Just p) x)

