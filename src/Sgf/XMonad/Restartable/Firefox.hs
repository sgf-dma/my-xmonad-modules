{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Restartable.Firefox
    ( FirefoxArgs
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

data FirefoxArgs    = FirefoxArgs
                        { _firefoxProfile       :: String
                        , _firefoxNoRemote      :: Bool
                        , _firefoxNewInstance   :: Bool 
                        }
  deriving (Show, Read, Typeable)
firefoxProfile :: LensA FirefoxArgs String
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
                        let xp = viewA firefoxProfile x
                            xr = viewA firefoxNoRemote x
                            xi = viewA firefoxNewInstance x
                        fmap concat . sequence $
                          [ unless' (null xp) (return ["-P", xp])
                          , when'    xr       (return ["--no-remote"])
                          , when'    xi       (return ["--new-instance"])
                          ]
    defaultArgs     = FirefoxArgs
                        { _firefoxProfile       = "default"
                        , _firefoxNoRemote      = False
                        , _firefoxNewInstance   = True
                        }

type Firefox        = Program FirefoxArgs
defaultFirefox :: Firefox
defaultFirefox      = setA progBin "firefox" defaultProgram

