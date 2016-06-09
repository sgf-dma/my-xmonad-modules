{-# LANGUAGE FlexibleContexts #-}

module Sgf.XMonad.Workspaces
    ( handleDefaultWorkspaces
    )
  where

import Data.Monoid
import Control.Monad

import XMonad
import qualified XMonad.StackSet as W

import Sgf.Data.List

handleDefaultWorkspaces :: Bool -- Apply at startup?
                           -> (WorkspaceId -> Bool)
                           -> XConfig l -> XConfig l
handleDefaultWorkspaces b p cf  = cf
    { handleEventHook = handleDefault p <+> handleEventHook cf
    , startupHook = when b (startupDefault p) >> startupHook cf
    }

-- Rise default workspace to the top of `hidden` workspace list in StackSet .
-- Note, that elements matching with predicate preserve their original order:
-- e.g. if predicate is (`elem` ["6", "7"]) and current hidden has [.., "7",
-- "6", ..], then workspace "6" will be used first. Also, if some of
-- workspaces matching with predicate are already visible, they'll not be in
-- `hidden` at all and can't be choosed.
riseDefault :: (WorkspaceId -> Bool) -> X ()
riseDefault p       = windows $ \ws@W.StackSet {W.hidden = hs } ->
      ws {W.hidden = riseElems (p . W.tag) hs}

-- startupHook for selecting default workspaces. Note about original workspace
-- order in `hidden` from `riseDefault` applies here too.
startupDefault :: (WorkspaceId -> Bool) -> X ()
startupDefault p    = windows $ \ws ->
    --let xs = zip (filter p . map W.tag $ W.workspaces ws)
    --             (map (W.tag . W.workspace) . W.screens $ ws)
    let xs = zip <$> (filter p . map W.tag . W.workspaces)
                 <*> (map (W.tag . W.workspace) . W.screens)
                 $ ws
    in  foldr (\(x, y) zws -> W.view x . W.view y $ zws) ws xs

-- Move specified workspaces to the head of `hidden` workspace list at Screen
-- change event. Then `rescreen` (from XMonad/Operations.hs), run by default
-- ConfigureEvent handler (`handle` from XMonad/Main.hsc), will choose first
-- ones from them for displaying on new Screen.
handleDefault :: (WorkspaceId -> Bool) -> Event -> X All
handleDefault p ConfigureEvent {ev_window = w} = do
    whenX (isRoot w) (riseDefault p)
    return (All True)
handleDefault _   _ = return (All True)

