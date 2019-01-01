{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards    #-}

module Sgf.XMonad.Config
    ( SessionConfig (..)
    , session
    , handleEwmh
    )
  where

import Data.Tagged

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Hooks.ManageDocks (AvoidStruts)
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Layout.ResizableTile
import XMonad.Hooks.DynamicLog (PP, shorten, xmobarColor)
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.Focus

import Sgf.Control.Lens
import Sgf.XMonad.Workspaces
import Sgf.XMonad.Fullscreen
import Sgf.XMonad.Pulse
import Sgf.XMonad.Docks
import Sgf.XMonad.Restartable
import Sgf.XMonad.Util.EntryHelper
import Sgf.XMonad.Lock
import Sgf.XMonad.Docks.Xmobar
import Sgf.XMonad.Docks.Trayer
import Sgf.XMonad.Restartable.Feh
import Sgf.XMonad.X11
import Sgf.XMonad.Util.EZConfig


-- Config for my modules.
data SessionConfig l = SessionConfig
                        { programs          :: [ProgConfig l]
                        , programHelpKey    :: Maybe (ButtonMask, KeySym)
                        , docksToggleKey    :: Maybe (ButtonMask, KeySym)
                        , defaultWorkspacesAtStartup :: Bool
                        , defaultWorkspaces :: WorkspaceId -> Bool
                        , activateFocusHook :: ManageHook
                        , newFocusHook      :: ManageHook
                        , focusLockKey      :: Maybe (ButtonMask, KeySym)
                        , lockWorkspace     :: WorkspaceId
                        , anotherWorkspace  :: WindowSet -> WorkspaceId
                        , lockKey           :: Maybe (ButtonMask, KeySym)
                        }
session :: forall l. LayoutClass l Window => SessionConfig l -> XConfig l
           -> XConfig (ModifiedLayout (ConfigurableBorder Ambiguity) (ModifiedLayout AvoidStruts l))
session SessionConfig{..}   =
                      let
                      -- 'handleProgs' may change new window placement
                      -- (workspace), so i should apply 'FocusHook' after
                      -- them.
                          mh = focusH . lock . progs
                      in  others . docks . defWs . mh
  where
    progs :: XConfig l -> XConfig l
    progs           = maybeAddModMask programHelpKey >>=
                        flip handleProgs programs
    lock  :: forall t. XConfig t -> XConfig t
    lock            = maybeAddModMask lockKey >>=
                        flip handleLock lockWorkspace
    focusH :: forall t. XConfig t -> XConfig t
    focusH cf       = addLockKey $
            cf  { logHook    = do
                    activateLogHook $
                        manageLockHook <> activateFocusHook
                    logHook cf
                , manageHook = manageLockHook <> newFocusHook <> manageHook cf
                }
          where
            manageLockHook :: ManageHook
            manageLockHook = manageLock lockWorkspace anotherWorkspace
            addLockKey :: XConfig t -> XConfig t
            addLockKey  = additionalKeys <*>
                            (maybeAddModMask focusLockKey >>=
                             flip maybeKey toggleLock)
    defWs  :: forall t. XConfig t -> XConfig t
    defWs           = handleDefaultWorkspaces
                        defaultWorkspacesAtStartup
                        defaultWorkspaces
    docks  :: forall t. LayoutClass t Window =>
              XConfig t -> XConfig (ModifiedLayout AvoidStruts t)
    docks           = maybeAddModMask docksToggleKey >>= handleDocks
    -- Functions not using SessionConfig (note type variable t).
    others :: forall t. LayoutClass t Window => XConfig t
              -> XConfig (ModifiedLayout (ConfigurableBorder Ambiguity) t)
    others          = handleFullscreen
                        . handleRecompile
                        . handlePulse
                        . handleEwmh

-- Reset _NET_SUPPORTED, so atoms added by display manager won't be inherited.
-- If xmonad needs any of them, i should add them explicitly.
handleEwmh :: XConfig l -> XConfig l
handleEwmh cf       = let cf' = ewmh cf
                      in  cf'   { startupHook = do
                                    resetNETSupported
                                    startupHook cf'
                                }

defDocks :: [ProgConfig l]
defDocks            = addDock trayer : map addDock [xmobar, xmobarAlt]

-- Note, that because i redefine PP, Xmobar implementation assumes, that
-- StdinReader is used in .xmobarrc, and opens pipe to xmobar. Thus, if
-- StdinReader does not actually used in template in .xmobarrc, xmonad will
-- freeze, when pipe fills up. See
-- https://wiki.haskell.org/Xmonad/Frequently_asked_questions#XMonad_is_frozen.21
-- .
-- Main xmobar, which does not have hiding (Strut toggle) key.
xmobar :: Xmobar
xmobar              = setA xmobarPP (Just xp) defaultXmobar
  where
    xp :: PP
    xp              = setA ppTitleL pt . setA ppHiddenL ph $ defaultXmobarPP
    pt :: String -> String
    pt              = xmobarColor "green" "" . shorten 50
    ph :: WorkspaceId -> String
    ph w            = "<action=`xdotool key super+" ++ w ++ "`>"
                        ++ w ++ "</action>"
-- Alternative xmobar, which has hiding (Strut toggle) key.
xmobarAlt :: Xmobar
xmobarAlt           = setA (xmobarProg . progArgs . xmobarConf) ".xmobarrcAlt"
                        . setA xmobarToggle (Just (shiftMask, xK_b))
                        $ defaultXmobar

trayer :: Trayer
trayer              = setA (trayerProg . progWait) 300000
                        . modifyA (trayerProg . progArgs . trayerArgs)
                          (++ [ "--edge", "top", "--align", "right"
                              , "--width", "10", "--height", "24"
                              , "--transparent", "true" , "--alpha", "0", "--tint", "0xc0c0c0"
                              , "--expand", "true"
                              ])
                        $ defaultTrayer


defPrograms :: [ProgConfig l]
defPrograms         = [addProg feh]

-- Will use `xsetroot -grey`, if no .fehbg found.
feh :: Feh
feh                 = defaultFeh

instance Default (SessionConfig l) where
    def = SessionConfig
            { programs          = defDocks ++ defPrograms
            , programHelpKey    = Just (0, xK_s)
            , docksToggleKey    = Just (0, xK_b)
            , defaultWorkspacesAtStartup = False
            , defaultWorkspaces = (`elem` ["7"])
            , activateFocusHook = manageFocus $ composeAll
                -- If `gmrun` is focused on workspace, on which activated
                -- window is, keep focus unchanged. But i may still switch
                -- workspace.
                [ focused (className =? "Gmrun")
                                --> keepFocus
                -- If firefox window is activated, do not switch workspace.
                -- But i may still switch focus on that workspace.
                , new (className =? "Iceweasel" <||> className =? "Firefox")
                                --> keepWorkspace
                -- Default behavior for activated windows: switch workspace
                -- and focus.
                , return True   --> switchWorkspace <+> switchFocus
                ]
            , newFocusHook      = manageFocus $ composeOne
                -- Always switch focus to `gmrun`.
                [ new (className =? "Gmrun")        -?> switchFocus
                -- And always keep focus on `gmrun`. Note, that another
                -- `gmrun` will steal focus from already running one.
                , focused (className =? "Gmrun")    -?> keepFocus
                -- If firefox dialog prompt (e.g. master password prompt) is
                -- focused on current workspace and new window appears here
                -- too, keep focus unchanged (note, used predicate: `newOnCur
                -- <&&> focused` is the same as `newOnCur <&&> focusedCur`,
                -- but is *not* the same as just `focusedCur` )
                , newOnCur <&&> focused
                    ((className =? "Iceweasel" <||> className =? "Firefox") <&&> isDialog)
                                                    -?> keepFocus
                -- Default behavior for new windows: switch focus.
                , return True                       -?> switchFocus
                ]
            , focusLockKey      = Nothing
            , lockWorkspace     = "lock"
            -- Choose most recently viewed workspace to place new window moved
            -- out from lock workspace.
            , anotherWorkspace  = head . filter (/= lockWorkspace def)
                                    . map W.tag . W.workspaces
            , lockKey           = Just (shiftMask, xK_z)
            }

layout :: Choose ResizableTall (Choose (Mirror ResizableTall) Full) Window
layout              = tiled ||| Mirror tiled ||| Full
  where
    tiled :: ResizableTall Window
    tiled           = ResizableTall nmaster delta ratio slaves
    nmaster :: Int
    nmaster         = 1
    delta :: Rational
    delta           = 3/100
    ratio :: Rational
    ratio           = 1/2
    slaves :: [Rational]
    slaves          = []

defKeys :: XConfig l -> [((ButtonMask, KeySym), X ())]
defKeys XConfig {modMask = m} =
      [ ((m,  xK_a), sendMessage MirrorShrink)
      , ((m,  xK_z), sendMessage MirrorExpand)
      ]

instance l ~ Choose ResizableTall (Choose (Mirror ResizableTall) Full)
         => Default (Tagged (SessionConfig t) (XConfig l))
  where
    def             = Tagged . (additionalKeys <*> defKeys)
                        $ def   { modMask = mod4Mask
                                , focusFollowsMouse = False
                                , clickJustFocuses = False
                                , layoutHook = layout
                                }

