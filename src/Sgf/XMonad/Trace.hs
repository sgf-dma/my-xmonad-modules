
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
      let ws' = W.view i ws
          fxs = [x | x <- W.index ws', x `M.member` (W.floating ws')]
          lxs = [x | x <- left    ws', x `notElem`  fxs]
          rxs = [x | x <- right   ws', x `notElem`  fxs]
          fx  = [x | x <- peek    ws', x `notElem`  fxs]
          --lxs = [x | x <- W.Stack . W.workspace . W.current
          --txs = [x | x <- xs, x `notElem` fxs]
      lts <- mapM showWindow lxs
      ft  <- mapM showWindow fx
      rts <- mapM showWindow rxs
      fs  <- mapM showWindow fxs
      trace $ "<" ++ i ++ "> tiled: left: "  ++ show lts
                              ++ ", focus: " ++ show ft
                              ++ ", right: " ++ show rts
      trace $ "<" ++ i ++ "> floating: " ++ show fs
  where
    -- Copy `with` from 'XMonad.StackSet', because it's not exported.
    with :: b -> (W.Stack a -> b) -> W.StackSet i l a s sd -> b
    with dflt f     = maybe dflt f . W.stack . W.workspace . W.current
    left :: W.StackSet i l a s sd -> [a]
    left            = with [] W.up
    right :: W.StackSet i l a s sd -> [a]
    right           = with [] W.down
    peek :: W.StackSet i l a s sd -> [a]
    peek            = with [] ((: []) . W.focus)

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

