
module Sgf.XMonad.Util.Run
    ( spawnPipe'
    , spawnPID'
    , spawn'
    , executeFileWithPATH
    , spawnPipeWithPATH'
    , spawnPIDWithPATH'
    , spawnWithPATH'
    )
  where

import XMonad.Core

import System.IO
import System.Posix.IO
import System.Posix.Process (executeFile)
import System.Posix.Types (ProcessID)
import Control.Monad (void)
import Control.Monad.Trans

import Sgf.System.Process


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

-- Modify PATH using supplied function and use resulting path list when
-- searching for command (if command name does not contain slashes). Search
-- behavior conforms to execve() behavior, but PATH search itself is performed
-- here with modified path list.
executeFileWithPATH :: FilePath -> Maybe ([FilePath] -> [FilePath])
                       -> [String] -> Maybe [(String, String)] -> IO a
executeFileWithPATH cmd mf args env = do
    c <- searchPATH cmd mf
    -- Path c 
    executeFile c True args env

-- Versions of spawnX', which allow to modify PATH.
spawnPipeWithPATH' :: MonadIO m => FilePath
                      -> Maybe ([FilePath] -> [FilePath]) -> [String]
                      -> m (Handle, ProcessID)
spawnPipeWithPATH' x mf xs  = io $ do
                        (rd, wr) <- createPipe
                        setFdOption wr CloseOnExec True
                        h <- fdToHandle wr
                        hSetBuffering h LineBuffering
                        p <- xfork $ do
                              _ <- dupTo rd stdInput
                              executeFileWithPATH x mf xs Nothing
                        closeFd rd
                        return (h, p)

spawnPIDWithPATH' :: MonadIO m => FilePath -> Maybe ([FilePath] -> [FilePath])
                     -> [String] -> m ProcessID
spawnPIDWithPATH' x mf xs   = xfork $ executeFileWithPATH x mf xs Nothing

spawnWithPATH' :: MonadIO m => FilePath -> [String] -> m ()
spawnWithPATH' x xs = void (spawnPID' x xs)

{-
-- Guess stack local bin path. Dereference link completely or this does not
-- matter?
stackLocalBin :: MonadIO m => m FilePath
stackLocalBin       = do
                        dir <- getXMonadDir
                        let binn = "xmonad-"++arch++"-"++os
                            bin  = dir </> binn
                        binl <- liftIO (readSymbolicLink bin)
                        return (takeDirectory binl)
-}

