
module Sgf.XMonad.Trace
    ( traceCurWorkspace
    , traceAllWindows
    , traceFloat
    , traceNew
    )
  where

import Data.Maybe
import Data.Monoid
import qualified Data.Map as M
import Control.Applicative
import Control.Monad

import XMonad
import qualified XMonad.StackSet as W


-- Log all windows (tiled and floating).
traceCurWorkspace :: X ()
traceCurWorkspace   = do
    trace "Windows on current workspace:"
    withWindowSet $ \ws -> do
      whenJust (W.stack . W.workspace . W.current $ ws) $ \s -> do
        ts <- mapM (runQuery title) (W.integrate s)
        trace $ "Tiled: " ++ show ts
      fs <- mapM (runQuery title) (M.keys . W.floating $ ws)
      unless (null fs) $ trace ("Floating: " ++ show fs)

traceAllWindows :: X ()
traceAllWindows     = do
    trace "All windows:"
    withWindowSet $ \ws -> forM_ (W.workspaces ws) $ \w ->
      whenJust (W.stack w) $ \s -> do
        ts <- mapM (runQuery title) (W.integrate s)
        trace $ "On workspace '" ++ W.tag w ++ "': " ++ show ts

-- Log windows, which would be made floating by default (particularly, by
-- `manage` from XMonad/Operations.hs), and why they would.
traceFloat :: ManageHook
traceFloat          = do
    w <- ask
    t <- title
    liftX $ withDisplay $ \d -> do
      sh <- io $ getWMNormalHints d w
      let isFixedSize = sh_min_size sh /= Nothing
                          && sh_min_size sh == sh_max_size sh
      isTransient <- isJust <$> io (getTransientForHint d w)
      let f | isFixedSize = "Fixed size window "
            | isTransient = "Transient window "
            | otherwise   = ""
      when (isFixedSize || isTransient) $
        trace (f ++ show w ++ " \"" ++ t ++ "\"")
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
    t <- title
    trace $ "New window: \"" ++ t ++ "\""
    return (Endo id)

