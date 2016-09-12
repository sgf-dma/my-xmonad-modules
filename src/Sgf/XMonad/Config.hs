{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Sgf.XMonad.Config
    ( SessionConfig (..)
    , SgfXConfig (..)
    , session
    )
  where

import Data.Tagged
import Control.Applicative

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Hooks.ManageDocks (AvoidStruts)
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig
import XMonad.Layout.ResizableTile
import XMonad.Hooks.ManageHelpers (isDialog)
import XMonad.Hooks.DynamicLog (PP, shorten, xmobarColor)

import Sgf.Control.Lens
import Sgf.XMonad.Workspaces
import Sgf.XMonad.Fullscreen
import Sgf.XMonad.Pulse
import Sgf.XMonad.Docks
import Sgf.XMonad.Restartable
import Sgf.XMonad.Focus
import Sgf.XMonad.Util.EntryHelper
import Sgf.XMonad.Lock
import Sgf.XMonad.Docks.Xmobar
import Sgf.XMonad.Docks.Trayer
import Sgf.XMonad.Restartable.Feh
import Sgf.XMonad.Hooks.ManageHelpers
import Sgf.XMonad.X11


(<.>) :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
(<.>) = liftA2 (.)

-- Config for my modules.
data SessionConfig l = SessionConfig
                        { programs          :: [ProgConfig l]
                        , programHelpKey    :: Maybe (ButtonMask, KeySym)
                        , docksToggleKey    :: Maybe (ButtonMask, KeySym)
                        , defaultWorkspacesAtStartup :: Bool
                        , defaultWorkspaces :: WorkspaceId -> Bool
                        , focusHook         :: FocusHook
                        , focusLockKey      :: Maybe (ButtonMask, KeySym)
                        , lockWorkspace     :: WorkspaceId
                        , anotherWorkspace  :: WindowSet -> WorkspaceId
                        , lockKey           :: Maybe (ButtonMask, KeySym)
                        }
toXConfig :: LayoutClass l Window => SessionConfig l -> XConfig l
             -> XConfig (ModifiedLayout AvoidStruts l)
toXConfig          =
    let defWs   = handleDefaultWorkspaces
                    <$> defaultWorkspacesAtStartup  <*> defaultWorkspaces
        fs      = handleFocusQuery <$> focusLockKey <*> focusHook
        ps      = handleProgs <$> programHelpKey    <*> programs
        ds      = handleDocks <$> docksToggleKey
        ls      = handleLock  <$> lockKey <*> lockWorkspace <*> anotherWorkspace
        -- `handleProgs` and `handleLock` may change new window placement, so
        -- i should apply `handleFocusQuery` after it.
    in  ds <.> defWs <.> fs <.> ls <.> ps

instance Default (Tagged (SessionConfig l) FocusHook) where
    -- The order matters! Because `composeOne` returns the first FocusHook,
    -- which won't be Nothing and does not try the others.
    def = Tagged $ composeOne
            -- Always switch focus to `gmrun`.
            [ new (className =? "Gmrun")    -?> switchFocus
            -- If `gmrun` or firefox dialog prompt (e.g. master password prompt) is
            -- focused on current workspace and new window appears here too, keep
            -- focus unchanged.
            , newOnCur <&&> focused (foldr1 (<||>)
                [ className =? "Gmrun"
                , (className =? "Iceweasel" <||> className =? "Firefox") <&&> isDialog
                ])
                                            -?> keepFocus
            , activated -?> composeAll
                -- If `gmrun` is focused on workspace, on which activated window is,
                -- keep focus unchanged. I may still switch workspace there.
                [ focused (className =? "Gmrun") --> keepFocus
                -- If firefox window is activated, do not switch workspace. I may
                -- still switch focus on that workspace.
                , new (className =? "Iceweasel" <||> className =? "Firefox")
                                            --> keepWorkspace
                -- Default behavior for activated windows: switch workspace and focus.
                , return True               --> switchWorkspace <+> switchFocus
                ]
            -- Default behavior for new windows: switch focus.
            , return True                   -?> switchFocus
            ]

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
                              , "--width", "10", "--height", "12"
                              , "--transparent", "true" , "--tint", "0x191970"
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
            , focusHook         = witness def (def :: SessionConfig a)
            , focusLockKey      = Nothing
            , lockWorkspace     = "lock"
            -- Choose most recently viewed workspace to place new window moved
            -- out from lock workspace.
            , anotherWorkspace  = head . filter (/= lockWorkspace def)
                                    . map W.tag . W.workspaces
            , lockKey           = Just (shiftMask, xK_z)
            }

handleEwmh :: XConfig l -> XConfig l
handleEwmh xcf      = xcf {startupHook = resetNETSupported >> startupHook xcf}

session :: LayoutClass l Window => SessionConfig l -> XConfig l
           -> XConfig (ModifiedLayout (ConfigurableBorder Ambiguity) (ModifiedLayout AvoidStruts l))
session cf          =
    handleFullscreen
      . handleRecompile
      . handlePulse
      . handleEwmh
      . toXConfig cf


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

newtype SgfXConfig l = SgfXConfig {fromSgfXConfig :: XConfig l}
instance l ~ Choose ResizableTall (Choose (Mirror ResizableTall) Full) => Default (SgfXConfig l) where
    def             = SgfXConfig . (additionalKeys <*> defKeys)
                        $ def {
                        -- Workspace "lock" is for xtrlock only and it is
                        -- inaccessible for workspace switch keys.
                        modMask = mod4Mask
                        , focusFollowsMouse = False
                        -- Do not set terminal here: edit `xterm` value
                        -- instead.  Because proper conversion from Program to
                        -- String (and back) should be done with respect to
                        -- shell escaping rules, it's simpler to just redefine
                        -- 'mod+shift+enter' to use Program value (`xterm`). I
                        -- set terminal here, though, to make it roughly match
                        -- to `xterm` value and to avoid conversion issues i
                        -- just throw away all arguments: at least it's safe..
                        , clickJustFocuses = False
                        , layoutHook = layout
                        }

