{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Restartable.Firefox
    ( FirefoxProfile (..)
    , FirefoxArgs
    , firefoxProfile
    , firefoxNoRemote
    , firefoxNewInstance
    , Firefox
    , defaultFirefox
    )
  where

import Data.Typeable
import Data.Function (on)

import Sgf.Data.List
import Sgf.Control.Lens
import Sgf.XMonad.Restartable

data FirefoxProfile = FfProfileManager
                    | FfProfile String
  deriving (Show, Read, Typeable, Eq)
data FirefoxArgs    = FirefoxArgs
                        { _firefoxProfile       :: FirefoxProfile
                        , _firefoxNoRemote      :: Bool
                        , _firefoxNewInstance   :: Bool
                        }
  deriving (Show, Read, Typeable)
firefoxProfile :: LensA FirefoxArgs FirefoxProfile
firefoxProfile f z@FirefoxArgs {_firefoxProfile = x}
                    = fmap (\x' -> z{_firefoxProfile = x'}) (f x)
firefoxNoRemote :: LensA FirefoxArgs Bool
firefoxNoRemote f z@FirefoxArgs {_firefoxNoRemote = x}
                    = fmap (\x' -> z{_firefoxNoRemote = x'}) (f x)
firefoxNewInstance :: LensA FirefoxArgs Bool
firefoxNewInstance f z@FirefoxArgs {_firefoxNewInstance = x}
                    = fmap (\x' -> z{_firefoxNewInstance = x'}) (f x)
instance Eq FirefoxArgs where
    (==)            = (==) `on` viewA firefoxProfile
instance Arguments FirefoxArgs where
    serialize x     = do
                        let xr = viewA firefoxNoRemote x
                            xi = viewA firefoxNewInstance x
                            xp = case (viewA firefoxProfile x) of
                                  FfProfile p -> unless' (null p) ["-P", p]
                                  FfProfileManager  -> ["-ProfileManager"]
                        fmap concat . sequence $
                          [ return xp
                          , when'    xr       (return ["--no-remote"])
                          , when'    xi       (return ["--new-instance"])
                          ]
    defaultArgs     = FirefoxArgs
                        { _firefoxProfile       = FfProfile "default"
                        , _firefoxNoRemote      = False
                        , _firefoxNewInstance   = True
                        }

type Firefox        = Program FirefoxArgs
defaultFirefox :: Firefox
defaultFirefox      = setA progBin "firefox" defaultProgram

