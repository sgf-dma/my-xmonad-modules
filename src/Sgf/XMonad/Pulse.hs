
module Sgf.XMonad.Pulse
    ( handlePulse
    )
  where

import Data.List
import System.Process
import Graphics.X11.ExtraTypes.XF86 -- For media keys.

import XMonad
import XMonad.Util.EZConfig


handlePulse :: XConfig l -> XConfig l
handlePulse         = additionalKeys <*> audioKeys
  where
    audioKeys :: XConfig l -> [((ButtonMask, KeySym), X ())]
    audioKeys XConfig {modMask = m} =
        [
        -- Audio keys.
        ((0, xF86XK_AudioLowerVolume), getDefaultSink >>= pulseVol VolDown)
        -- FIXME: This really not exactly what i want. I want, that if sound is
        -- muted, one VolUp only unmutes it. Otherwise, just VolUp-s.
        , ((0, xF86XK_AudioRaiseVolume), getDefaultSink >>= \s -> pulseVol VolOn s >> pulseVol VolUp s)
        , ((0, xF86XK_AudioMute       ), getDefaultSink >>= pulseVol VolOff)
        ]

type Sink   = Int
data Vol    = VolUp
            | VolDown
            | VolOn
            | VolOff
  deriving (Eq, Show)

-- Use RUNNING sink, if any, as default, otherwise use sink 0.
getDefaultSink :: X (Maybe Sink)
getDefaultSink      = do
    uninstallSignalHandlers
    l <- io $ readProcess "pactl" ["list","short", "sinks"] []
    installSignalHandlers
    let s = maybe (Just 0) id . fmap (read . head)
              . find ((== "RUNNING") . last) . map words . lines
              $ l
    return s

-- Volume up/down and mute/unmute using pulseaudio.
pulseVol :: Vol -> Maybe Sink -> X ()
pulseVol v
  | v == VolUp      = vol "+"
  | v == VolDown    = vol "-"
  where
    vol :: String -> Maybe Sink -> X ()
    vol p           = maybe (return ()) $ \x -> do
                        spawn $
                          "pactl set-sink-volume " ++ show x ++ " -- " ++ p ++ "5%"
pulseVol v
  | v == VolOn      = mute "off"
  | v == VolOff     = mute "on"
  where
    mute :: String -> Maybe Sink -> X ()
    mute m          = maybe (return ()) $ \x -> do
                        spawn $
                          "pactl set-sink-mute " ++ show x ++ " -- " ++ m

