{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Restartable.Firefox
    ( FirefoxArgs
    , firefoxProfile
    -- , firefoxNoRemote
    , Firefox
    , defaultFirefox
    )
  where

import Data.Typeable
import Data.Function (on)

import Sgf.Data.List
import Sgf.Control.Lens
import Sgf.XMonad.Restartable

-- I've added record for "--no-remote" only for completeness, there is almost
-- no sense to launch FF with profile set and *without* "--no-remote".
data FirefoxArgs    = FirefoxArgs
                        { _firefoxProfile   :: String
                        , _firefoxNoRemote  :: Bool
                        }
  deriving (Show, Read, Typeable)
firefoxProfile :: LensA FirefoxArgs String
firefoxProfile f z@(FirefoxArgs {_firefoxProfile = x})
                    = fmap (\x' -> z{_firefoxProfile = x'}) (f x)
firefoxNoRemote :: LensA FirefoxArgs Bool
firefoxNoRemote f z@(FirefoxArgs {_firefoxNoRemote = x})
                    = fmap (\x' -> z{_firefoxNoRemote = x'}) (f x)
instance Eq FirefoxArgs where
    (==)            = (==) `on` viewA firefoxProfile
instance Arguments FirefoxArgs where
    serialize x     = let xp = viewA firefoxProfile x
                          xr = viewA firefoxNoRemote x
                      in     whenL (not . null $ xp) ["-P", xp]
                          ++ whenL xr                ["-no-remote"]
    defaultArgs     = FirefoxArgs
                        { _firefoxProfile = "default"
                        , _firefoxNoRemote = True
                        }

type Firefox        = Program FirefoxArgs
defaultFirefox :: Firefox
defaultFirefox      = setA progBin "firefox" defaultProgram

