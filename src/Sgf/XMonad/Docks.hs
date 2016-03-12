{-# LANGUAGE FlexibleContexts #-}

module Sgf.XMonad.Docks
    ( addDock
    , handleDocks
    , DockClass (..)
    , ppCurrentL
    , ppVisibleL
    , ppHiddenL
    , ppHiddenNoWindowsL
    , ppUrgentL
    , ppSepL
    , ppWsSepL
    , ppTitleL
    , ppLayoutL
    , ppOrderL
    , ppSortL
    , ppExtrasL
    , ppOutputL
    )
  where

import Data.Maybe
import Data.List
import Data.Monoid
import Control.Monad.State
import Control.Applicative

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Hooks.ManageDocks hiding (docksEventHook)
import XMonad.Hooks.DynamicLog
import XMonad.Layout.LayoutModifier (ModifiedLayout)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.WindowProperties (getProp32s)
import Foreign.C.Types (CLong)

import Sgf.Control.Lens
import Sgf.XMonad.Restartable


class ProcessClass a => DockClass a where
    dockToggleKey   :: a -> Maybe (ButtonMask, KeySym)
    dockToggleKey   = const Nothing
    ppL             :: LensA a (Maybe PP)
    ppL             = nothingL

addDock :: (RestartClass a, DockClass a, LayoutClass l Window) =>
           a -> ProgConfig l
addDock d           = ProgConfig
      -- Raise all managed by xmonad windows (from all workspaces) on top of X
      -- stack. When i restart docks, their windows will be placed in X stack
      -- above application windows opened earlier.  And when i switch Struts
      -- off, these old application windows will go under docks.  To fix this,
      -- i will raise all managed windows. This may change windows order on
      -- screen, but only for a short time, when new dock opens. See
      -- `raiseAllWindows` for details.
      { progManageHook  = liftX (getProcess d) >>=
                            manageProcess (liftX raiseAllWindows >> manageP d)
      -- Launch dock process: PP does not saved in Extensible State and should
      -- be reinitialized before start by `doLaunchP` . To make this happen,
      -- functions used to start process should use `withProcessP` (not just
      -- `withProcess`), which `mappend`-s value used for searching in
      -- Extensible State to found value.
      , progStartupHook =  when (launchAtStartup d) (doLaunchP d)
      -- Keys for launching and toggling Struts of this Dock.
      , progKeys        = liftA2 (++) <$> launchProg <*> toggleDock $ d
      -- Log to dock according to its PP .
      , progLogHook     = dockLog d
      -- And show key used for launching dock.
      , showProgKeys    = showKeys d
      }

-- Raise all managed by xmonad windows (from all workspaces) on top of X
-- stack. When i restart docks, their windows will be placed in X stack
-- above application windows opened earlier.  And when i switch Struts off,
-- these old application windows will go under docks. To render any
-- operation (change in StackSet) xmonad calls `windows` from
-- XMonad/Operations.hs, which calls `restackWindows` to restack managed
-- windows in X, and `restackWindows` does not change the position of the
-- the first restacked window
-- (https://tronche.com/gui/x/xlib/window/XRestackWindows.html). So, to lay
-- all managed windows above docks, i should raise either that first
-- restacked window at each workspace to the top of X stack (then
-- `restackWindows` will raise other managed windows from corresponding
-- workspace after it) or raise all managed windows.
--
-- Restacked windows list is `map fst (flt ++ rs)`, where 'flt' is floating
-- windows and rs - tiling.  Windows order in 'flt' is determined by
-- `integrate`. Windows order in 'rs' is determined by layout (`runLayout`).
-- Thus, the only way to find first window in 'rs' is to run `runLayout`,
-- which may (theoretically) affect X state (note, that i don't want to also
-- run `updateLayout`).
--
-- On the other hand, i may just raise all windows. It may change windows
-- order on screen, but only for a short time, when new dock opens. This
-- function is called from ManageHook and runs in X monad, thus `raiseWindow`
-- will run before `windows` call in `manage` (when pure function returned by
-- ManageHook is evaluated).  Then `windows` will overwrite window layout for
-- current workspace according to `runLayout`. For other workspaces `windows`
-- will overwrite window layout, when processing workspace switch.
raiseAllWindows :: X ()
raiseAllWindows     = withDisplay $ \d ->
    withWindowSet (mapM_ (io . raiseWindow d) . W.allWindows)

toggleDock :: DockClass a => a -> XConfig l -> [((ButtonMask, KeySym), X ())]
toggleDock x (XConfig {modMask = m}) = maybeToList $ do
    (mk, k) <- dockToggleKey x
    return ((m .|. mk, k), toggleProcessStruts x)

-- Handle all dock applications properly and add a key for toggling visibility
-- (Struts) of all docks.
handleDocks :: LayoutClass l Window => Maybe (ButtonMask, KeySym)
               -> XConfig l -> XConfig (ModifiedLayout AvoidStruts l)
handleDocks mt cf   = additionalKeys <*> toggleAllDocks mt $ cf
      -- First, de-manage dock applications.
      { manageHook = manageDocks <+> manageHook cf
      -- Then refresh screens after new dock appears.
      , handleEventHook = docksEventHook <+> handleEventHook cf
      -- Reduce Rectangle available for other windows according to Struts.
      , layoutHook = avoidStruts (layoutHook cf)
      }
  where
    toggleAllDocks :: Maybe (ButtonMask, KeySym) -> XConfig l
                      -> [((ButtonMask, KeySym), X ())]
    toggleAllDocks (Just (mk, k)) XConfig{modMask = m} =
                            [((m .|. mk, k), sendMessage ToggleStruts)]
    toggleAllDocks Nothing _ = []

-- Toggle struts for ProcessClass instance.
toggleProcessStruts :: ProcessClass a => a -> X ()
toggleProcessStruts y = do
    mx <- getProcess y
    flip (maybe (return ())) mx $ \x -> do
      ws <- findWins x
      ss <- mapM getStrut ws
      let ds = nub . map (\(s, _, _, _) -> s) . concat $ ss
      mapM_ (sendMessage . ToggleStrut) ds

-- Copy from XMonad.Hooks.ManageDocks .
type Strut = (Direction2D, CLong, CLong, CLong)

-- | Gets the STRUT config, if present, in xmonad gap order
getStrut :: Window -> X [Strut]
getStrut w = do
    msp <- getProp32s "_NET_WM_STRUT_PARTIAL" w
    case msp of
        Just sp -> return $ parseStrutPartial sp
        Nothing -> maybe [] parseStrut <$> getProp32s "_NET_WM_STRUT" w
 where
    parseStrut xs@[_, _, _, _] = parseStrutPartial . take 12 $ xs ++ cycle [minBound, maxBound]
    parseStrut _ = []

    parseStrutPartial [l, r, t, b, ly1, ly2, ry1, ry2, tx1, tx2, bx1, bx2]
     = filter (\(_, n, _, _) -> n /= 0)
        [(L, l, ly1, ly2), (R, r, ry1, ry2), (U, t, tx1, tx2), (D, b, bx1, bx2)]
    parseStrutPartial _ = []
-- End copy from XMonad.Hooks.ManageDocks .

-- docksEventHook version from xmobar tutorial (5.3.1 "Example for using the
-- DBus IPC interface with XMonad"), which refreshes screen on unmap events as
-- well.
docksEventHook :: Event -> X All
docksEventHook e = do
    when (et == mapNotify || et == unmapNotify) $
        whenX ((not `fmap` isClient w) <&&> runQuery checkDock w) refresh
    return (All True)
    where w  = ev_window e
          et = ev_event_type e

dockLog :: DockClass a => a ->  X ()
dockLog y           = do
    mx <- getProcess y
    fromMaybe (return ()) $ do
      x <- mx
      pp <- viewA ppL x
      return (dynamicLogWithPP pp)


-- Lenses to PP.
ppCurrentL :: LensA PP (WorkspaceId -> String)
ppCurrentL f z@(PP {ppCurrent = x})
                    = fmap (\x' -> z{ppCurrent = x'}) (f x)
ppVisibleL :: LensA PP (WorkspaceId -> String)
ppVisibleL f z@(PP {ppVisible = x})
                    = fmap (\x' -> z{ppVisible = x'}) (f x)
ppHiddenL :: LensA PP (WorkspaceId -> String)
ppHiddenL f z@(PP {ppHidden = x})
                    = fmap (\x' -> z{ppHidden = x'}) (f x)
ppHiddenNoWindowsL :: LensA PP (WorkspaceId -> String)
ppHiddenNoWindowsL f z@(PP {ppHiddenNoWindows = x})
                    = fmap (\x' -> z{ppHiddenNoWindows = x'}) (f x)
ppUrgentL :: LensA PP (WorkspaceId -> String)
ppUrgentL f z@(PP {ppUrgent = x})
                    = fmap (\x' -> z{ppUrgent = x'}) (f x)
ppSepL :: LensA PP String
ppSepL f z@(PP {ppSep = x})
                    = fmap (\x' -> z{ppSep = x'}) (f x)
ppWsSepL :: LensA PP String
ppWsSepL f z@(PP {ppWsSep = x})
                    = fmap (\x' -> z{ppWsSep = x'}) (f x)
ppTitleL :: LensA PP (String -> String)
ppTitleL f z@(PP {ppTitle = x})
                    = fmap (\x' -> z{ppTitle = x'}) (f x)
ppLayoutL :: LensA PP (String -> String)
ppLayoutL f z@(PP {ppLayout = x})
                    = fmap (\x' -> z{ppLayout = x'}) (f x)
ppOrderL :: LensA PP ([String] -> [String])
ppOrderL f z@(PP {ppOrder = x})
                    = fmap (\x' -> z{ppOrder = x'}) (f x)
ppSortL :: LensA PP (X ([WindowSpace] -> [WindowSpace]))
ppSortL f z@(PP {ppSort = x})
                    = fmap (\x' -> z{ppSort = x'}) (f x)
ppExtrasL :: LensA PP [X (Maybe String)]
ppExtrasL f z@(PP {ppExtras = x})
                    = fmap (\x' -> z{ppExtras = x'}) (f x)
ppOutputL :: LensA PP (String -> IO ())
ppOutputL f z@(PP {ppOutput = x})
                    = fmap (\x' -> z{ppOutput = x'}) (f x)

