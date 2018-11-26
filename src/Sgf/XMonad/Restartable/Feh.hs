{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Restartable.Feh
    ( Feh
    , fehProg
    , fehBg
    , defaultFeh
    )
  where

import Data.Typeable
import Control.Monad.Trans
import System.FilePath
import System.Directory

import Sgf.Control.Lens
import Sgf.XMonad.Restartable
import Sgf.XMonad.Restartable.Shell


-- There is no arguments for feh, i just execute content of ~/.fenbg through
-- shell . Here derived Eq instance will distinguish Feh values with different
-- fehBg file path, but content of this file does not matter (because all
-- Shell values are equal).
data Feh            = Feh {_fehProg :: Shell, _fehBg :: FilePath}
  deriving (Show, Read, Typeable, Eq)
fehProg :: LensA Feh Shell
fehProg f z@Feh {_fehProg = x}
                    = fmap (\x' -> z{_fehProg = x'}) (f x)
fehBg :: LensA Feh FilePath
fehBg f z@Feh {_fehBg = x}
                    = fmap (\x' -> z{_fehBg = x'}) (f x)
defaultFeh :: Feh
defaultFeh          = Feh {_fehProg = defaultShell, _fehBg = ".fehbg"}

instance Semigroup Feh where
    x <> y          = modifyA fehProg (viewA fehProg x <>) y
instance Monoid Feh where
    mappend         = (<>)
    mempty          = defaultFeh
instance ProcessClass Feh where
    pidL            = fehProg . pidL
instance RestartClass Feh where
    runP x          = do
        cmd <- liftIO $ do
          h <- getHomeDirectory
          let f = h </> viewA fehBg x
          b <- doesFileExist f
          if b
            then readFile f
            else return "xsetroot -grey"
        modifyAA fehProg runP (setA (fehProg . progArgs . shellCmd) cmd x)

