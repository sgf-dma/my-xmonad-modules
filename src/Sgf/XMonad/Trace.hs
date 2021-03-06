
module Sgf.XMonad.Trace
    ( showWindow
    , traceWorkspace
    , traceCurrentWorkspace
    , traceAllWorkspaces
    , traceFloat
    , traceNew
    , traceWindowSet
    )
  where

import Data.Maybe
import Data.List
import Data.Monoid
import qualified Data.Map as M
import Control.Monad

import XMonad
import qualified XMonad.StackSet as W


-- Show window title with id (may be used by `xwininfo -id`).
showWindow :: Window -> X String
showWindow w        = do
    t <- runQuery title w
    return ("'" ++ t ++ "' - " ++ show w)

-- Log tiled and floating windows on specified workspace.
traceWorkspace :: WorkspaceId -> X ()
traceWorkspace i    = withWindowSet $ \ws -> do
      let xs  = W.index . W.view i $ ws
          fxs = [x | x <- xs, M.member x (W.floating ws)]
          txs = [x | x <- xs, x `notElem` fxs]
      ts <- mapM showWindow txs
      fs <- mapM showWindow fxs
      trace $ "<" ++ i ++ "> tiled: "    ++ show ts
      trace $ "<" ++ i ++ "> floating: " ++ show fs

-- Log all windows on current workspace.
traceCurrentWorkspace :: X ()
traceCurrentWorkspace   = do
    trace "Windows on current workspace: "
    withWindowSet $ traceWorkspace . W.currentTag
-- Log all windows on all workspaces.
traceAllWorkspaces :: X ()
traceAllWorkspaces      = do
    trace "All windows: "
    withWindowSet $ mapM_ (traceWorkspace . W.tag) . W.workspaces

-- Log windows, which would be made floating by default (particularly, by
-- `manage` from XMonad/Operations.hs), and why they would.
traceFloat :: ManageHook
traceFloat          = do
    w <- ask
    xs <- liftX (showWindow w)
    liftX $ withDisplay $ \d -> do
      sh <- io $ getWMNormalHints d w
      let isFixedSize = sh_min_size sh /= Nothing
                          && sh_min_size sh == sh_max_size sh
      isTransient <- isJust <$> io (getTransientForHint d w)
      let f | isFixedSize = "Fixed size window "
            | isTransient = "Transient window "
            | otherwise   = ""
      when (isFixedSize || isTransient) $
        trace (f ++ xs)
    return idHook

-- Note, that i can't print new window workspace from ManageHook: user's
-- ManageHook evaluated in `manage` in XMonad/Operations.hs *before* new
-- window is added to 'windowset' in XState. Then, `windows` is called with
-- composition of (WindowSet -> WindowSet) functions: first function adds new
-- window to WindowSet, second is the one returned by user's ManageHook.
-- Thus, all monadic actions in ManageHook (like `trace`) run before new
-- window is added to current WindowSet and have not access to new window
-- workspace, but user's ManageHook pure function (WindowSet -> WindowSet) run
-- after and can find workspace of new window.
traceNew :: ManageHook
traceNew            = do
    xs <- ask >>= liftX . showWindow
    trace $ "New window: " ++ xs
    return (Endo id)

-- Log current, visible and hidden workspaces from WindowSet.
traceWindowSet :: X ()
traceWindowSet      = withWindowSet $ \W.StackSet
                                         { W.current = v
                                         , W.visible = vs
                                         , W.hidden = hs } -> do
    trace ("Current: " ++ (W.tag . W.workspace $ v))
    trace ("Visible: " ++ (showWS . map W.workspace $ vs))
    trace ("Hidden: "  ++ showWS hs)
  where
    showWS :: [W.Workspace WorkspaceId (Layout Window) Window] -> String
    showWS          = intercalate ", " . map W.tag

