{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Docks.Trayer
    ( TrayerArgs
    , trayerArgs
    , Trayer
    , trayerProg
    , defaultTrayer
    )
  where

import Data.Typeable

import Sgf.Control.Lens
import Sgf.XMonad.Restartable
import Sgf.XMonad.Docks

-- I do not need any comparison or interpretation for trayer arguments, so i
-- just wrap [String].
newtype TrayerArgs  = TrayerArgs {_trayerArgs :: [String]}
  deriving (Show, Read, Typeable)
trayerArgs :: LensA TrayerArgs [String]
trayerArgs f z@TrayerArgs {_trayerArgs = x}
                    = fmap (\x' -> z{_trayerArgs = x'}) (f x)
instance Eq TrayerArgs where
    _ == _          = True
instance Arguments TrayerArgs where
    serialize (TrayerArgs args) = return args
    defaultArgs     = TrayerArgs
                        [ "--SetDockType", "true"
                        , "--SetPartialStrut", "true"
                        ]

-- I use newtype to redefine doLaunchP of Program to execute `restartP`.
newtype Trayer      = Trayer {_trayerProg :: Program TrayerArgs}
  deriving (Eq, Show, Read, Typeable)
trayerProg :: LensA Trayer (Program TrayerArgs)
trayerProg f z@Trayer {_trayerProg = x}
                    = fmap (\x' -> z{_trayerProg = x'}) (f x)
defaultTrayer :: Trayer
defaultTrayer       = Trayer $ setA progBin "trayer" defaultProgram
instance Monoid Trayer where
    (Trayer x) `mappend` (Trayer y) = Trayer (x `mappend` y)
    mempty          = Trayer mempty
instance ProcessClass Trayer where
    pidL f (Trayer x)   = Trayer <$> pidL f x
instance RestartClass Trayer where
    runP (Trayer x)     = Trayer <$> runP x
    doLaunchP           = restartP
instance DockClass Trayer where

