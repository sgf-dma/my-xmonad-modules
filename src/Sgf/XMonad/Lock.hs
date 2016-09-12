
module Sgf.XMonad.Lock
    ( handleLock
    , manageLock
    )
  where

import System.Process

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig

import Sgf.XMonad.Focus


handleLock :: Maybe (ButtonMask, KeySym)    -- Lock key.
              -> WorkspaceId                -- Lock workspace name.
              -> (WindowSet -> WorkspaceId) -- Workspace, where to move new
                                            -- window from lock workspace.
              -> XConfig l -> XConfig l
handleLock ml lockWs anotherWs xcf = additionalKeys <*> addLockKey ml $ xcf
        { workspaces = workspaces xcf ++ [lockWs]
        , manageHook = manageLock lockWs anotherWs <+> manageHook xcf
        }
  where
    addLockKey :: Maybe (ButtonMask, KeySym)
                  -> XConfig l -> [((ButtonMask, KeySym), X ())]
    addLockKey (Just (mk, k)) XConfig{modMask = m}  = [((m .|. mk, k), lock)]
    addLockKey Nothing        _                     = []

-- Moves away new window from lock workspace regardless of current workspace
-- and focus.
manageLock :: WorkspaceId -> (WindowSet -> WorkspaceId) -> ManageHook
manageLock lockWs anotherWs =
    manageFocus def (newOn lockWs --> moveTo anotherWs)

moveTo :: (WindowSet -> WorkspaceId) -> FocusHook
moveTo anotherWs    = new $ asks pure >>= doF . (shiftWin <*> anotherWs <*>)
  where
    shiftWin :: WindowSet -> WorkspaceId -> Window -> WindowSet
    shiftWin ws i w = W.shiftWin i w ws

-- FIXME: When i wait for xtrlock process to terminate, i always come back to
-- old workpace, where i was before pressing lock keys (regardless of
-- workpspace switching code) and all windows are closed on it. Why? But it
-- does not close windows, if i comment out code, obtaining current
-- workspace..
-- 
-- Get current workspace tag, then switch to workspace "lock" (dedicated for
-- "xtrlock" and inaccessible for workspace switch keys) and lock. After
-- unlocking return back to workspace, where i was before.
lock :: X ()
lock                = do
                        --wi <- gets curWsId
                        windows (W.greedyView "lock")
                        spawn "xtrlock"
                        --p <- liftIO xtrlock
                        --liftIO (waitForProcess p)
                        --windows (W.greedyView wi)
                        return ()
  where
    -- Get current workspace tag.
    curWsId :: XState -> WorkspaceId
    curWsId         = W.tag . W.workspace . W.current . windowset
    xtrlock :: IO ProcessHandle
    xtrlock         = do
                        (_, _, _, p) <-
                            createProcess (proc "/usr/bin/xtrlock" [])
                        return p

