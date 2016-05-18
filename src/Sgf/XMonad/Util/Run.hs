{-# LANGUAGE FlexibleContexts #-}

module Sgf.XMonad.Util.Run
    ( spawnPipe'
    , spawnPID'
    , spawn'
    )
  where

import XMonad.Core

import Data.Maybe
import System.IO
import System.IO.Error
import System.Info
import System.FilePath
import System.Directory
import System.Posix.IO
import System.Posix.Process
import System.Posix.Types (ProcessID)
import System.Posix.Files
import Control.Exception
import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Cont
import Control.Monad.State

import Sgf.Data.List

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
stackLocalBin       = do
                        dir <- getXMonadDir
                        let binn = "xmonad-"++arch++"-"++os
                            bin  = dir </> binn
                        binl <- liftIO (readSymbolicLink bin)
                        return (takeDirectory binl)

{-
data SearchPathes   = NoSearch
                    | OnlySearch [FilePath]
                    | OnlyPATH
                    | BeforePATH [FilePath]
                    | AfterPATH [FilePath]

                    | Search Before [FilePath]
                    | Search Only


                    ([FilePath] -> [FilePath])
-}

--Nothing -> cmd
--Just f  -> findM isExecutable getSearchPath

executeFileWithSerch :: FilePath -> Maybe ([FilePath] -> [FilePath])
                        -> [String] -> Maybe [(String, String)] -> IO a
executeFileWithSerch cmd mf args env = do
    mcmd <- runMaybeT $ do
      liftIO (hPrint stderr "A")
      guard ('/' `notElem` cmd)
      liftIO (hPrint stderr "B")
      f <- maybeT mf
      liftIO (hPrint stderr "C")
      --MaybeT (getSearchPath >>= findM isExecutable . map (</> cmd) . f)
      (m, e) <- runStateT
        (liftIO getSearchPath >>= findM isExecutableM . map (</> cmd) . f)
        (mkIOError doesNotExistErrorType "huh" Nothing Nothing)
      maybe (throw e) return m
    liftIO (hPrint stderr "D")
    -- executeFile searches for cmd in current directory.. Should i specify
    -- it? Or hardcode adding '.' to search path?
    executeFile (fromMaybe cmd mcmd) False args env
  where
    maybeT :: Monad m => Maybe a -> MaybeT m a
    maybeT          = MaybeT . return

isExecutableWithSearch :: MonadIO m => FilePath -> ([FilePath] -> [FilePath]) -> m (Either IOError FilePath)
isExecutableWithSearch cmd f = do
    (m, e) <- runStateT
      (liftIO getSearchPath >>= findM isExecutableM . map (</> cmd) . f)
      (mkIOError doesNotExistErrorType "huh" Nothing Nothing)
    return (maybe (throw e) Right m)

isExecutableM :: (MonadIO m, MonadState IOError m) => FilePath -> m Bool
isExecutableM f     = do
                        -- Use try .
                        e <- liftIO $ catch
                          (getPermissions f >>= return . Right . executable)
                          (\e -> hPrint stderr e >> return (Left e))
                        case e of
                          Right b -> return b
                          Left  e -> put e >> return False


isExecutable :: FilePath -> IO Bool
isExecutable f  = catch (getPermissions f >>= return . executable)
                        (\e -> hPrint stderr e >> return (const False (e :: IOError)))

-- Execute file, which tries stack's local bin path, if binary not found in
-- PATH.
executeFile'stack :: FilePath -> Bool -> [String] -> Maybe [(String, String)] -> IO a
executeFile'stack cmd search args env   =
                        catch (executeFile cmd search args env) $ \e ->
                          if search && isDoesNotExistError e
                            then do
                              h <- stackLocalBin
                              let x'stack = h </> cmd
                              b <- isExecutable x'stack
                              -- annotateIOError for existing exception?
                              if b 
                                then executeFile x'stack search args env
                                --else throw $ annotateIOError e "Neither in stack" Nothing (Just x'stack)
                                else throw  e
                            else throw e
  where
    isExecutable :: FilePath -> IO Bool
    isExecutable f  = catch (getPermissions f >>= return . executable)
                            (\e -> return (const False (e :: IOError)))


spawnPID'stack :: MonadIO m => FilePath -> [String] -> m ProcessID
spawnPID'stack x xs = xfork $ catch (executeFile x True xs Nothing) $ \e ->
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


