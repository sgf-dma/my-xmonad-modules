{-# LANGUAGE FlexibleContexts #-}

module Sgf.XMonad.Fullscreen
    ( handleFullscreen
    )
  where

import XMonad
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Hooks.EwmhDesktops (fullscreenEventHook)
import XMonad.Hooks.SetWMName


-- Modify layoutHook to remove borders around floating windows covering whole
-- screen and around tiled windows in non-ambiguous cases. Also, add event
-- hook to detect windows going to fullscreen using _NET_WM_STATE protocol
-- (EWMH).
handleFullscreen :: LayoutClass l Window => XConfig l
                    -> XConfig (ModifiedLayout (ConfigurableBorder Ambiguity) l)
handleFullscreen cf = cf
    { layoutHook        = lessBorders OtherIndicated (layoutHook cf)
    , handleEventHook   = fullscreenEventHook <+> handleEventHook cf
    -- Note, the order in startupHook is important: `ewmh` function from
    -- XMonad.Hooks.EwmhDesktops also sets some atoms in _NET_SUPPORTED and
    -- uses 'propModeReplace'. Thus, it should be applied (if ever) *before*
    -- to not overwrite fullscreen support.
    , startupHook       = startupHook cf >> fullscreenStartupHook
    }

-- Setting WMName adds two atoms '_NET_SUPPORTING_WM_CHECK' and '_NET_WM_NAME'
-- to '_NET_SUPPORTED' and required for making youtube fullscreen work.
fullscreenStartupHook :: X ()
fullscreenStartupHook   = setSupportedFullscreen >> setWMName "xmonad"

setSupportedFullscreen :: X ()
setSupportedFullscreen  = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- getAtom "_NET_WM_STATE_FULLSCREEN"
    io $ changeProperty32 dpy r a c propModeAppend [fromIntegral supp]

