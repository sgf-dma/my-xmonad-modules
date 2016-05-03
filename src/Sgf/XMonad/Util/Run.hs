
module Sgf.XMonad.Util.Run
    ( spawnPipe'
    , spawnPID'
    , spawn'
    )
  where

import XMonad.Core

import System.IO
import System.Posix.IO
import System.Posix.Process (executeFile)
import System.Posix.Types (ProcessID)
import Control.Monad
import Control.Monad.Trans

-- Variants of spawnPipe and spawnPID running process directly (not through
-- shell).
spawnPipe' :: MonadIO m => FilePath -> [String] -> m (Handle, ProcessID)
spawnPipe' x xs     = io $ do
                        (rd, wr) <- createPipe
                        setFdOption wr CloseOnExec True
                        h <- fdToHandle wr
                        hSetBuffering h LineBuffering
                        p <- xfork $ do
                              _ <- dupTo rd stdInput
                              executeFile x True xs Nothing
                        closeFd rd
                        return (h, p)

spawnPID' :: MonadIO m => FilePath -> [String] -> m ProcessID
spawnPID' x xs      = xfork $ executeFile x True xs Nothing

spawn' :: MonadIO m => FilePath -> [String] -> m ()
spawn' x xs         = void (spawnPID' x xs)

-- Guess stack local bin path. Dereference link completely or this does not
-- matter?
stackLocalBin :: MonadIO m => m FilePath
stackLocalBin	    = do
			readSymbolicLink


-- Execute file, which tries stack's local bin path, if binary not found in
-- PATH.
executeFile' ...

spawnPID'Stack :: MonadIO m => FilePath -> [String] -> m ProcessID
spawnPID'Stack x xs = xfork $ catch (executeFile x True xs Nothing) $ \e ->
			  if isDoesNotExistError e
			    then do
			      h <- getHomeDirectory
			      let localBinPath = h </> ".local/bin"
			          x'stack = localBinPath </> x
			      b <- isExecutable x'stack
			      if b 
			        then executeFile "cls" True [] Nothing
				else hPrint stderr (x ++ " can't be executed and not found in stack's local bin path: '" ++ localBinPath ++ "'\n" ++ show e)
			    else throw e
  where
    isExecutable :: FilePath -> IO Bool
    isExecutable f  = catch (getPermissions f >>= return . executable)
                            (\e -> return (const False (e :: IOError)))


