{-# LANGUAGE FlexibleContexts #-}

module Sgf.XMonad.Fullscreen
    ( handleFullscreen
    )
  where

import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)


-- Modify layoutHook to remove borders around floating windows covering whole
-- screen and around tiled windows in non-ambiguous cases. Also, add event
-- hook to detect windows going to fullscreen using _NET_WM_STATE protocol
-- (EWMH).
handleFullscreen :: LayoutClass l Window => XConfig l
                    -> XConfig (ModifiedLayout (ConfigurableBorder Ambiguity) l)
handleFullscreen cf = cf
    { layoutHook        = lessBorders OtherIndicated (layoutHook cf)
    , handleEventHook   = fullscreenEventHook <+> handleEventHook cf
    }

