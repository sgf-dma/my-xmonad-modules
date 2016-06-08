{-# LANGUAGE FlexibleContexts #-}

module Sgf.XMonad.Lock
    ( handleLock
    )
  where

import Data.Maybe
import System.Process

import XMonad
import qualified XMonad.StackSet as W
import XMonad.Util.EZConfig

handleLock :: XConfig l -> XConfig l
handleLock xcf      = additionalKeys <*> lockKeys $ xcf
        {
        workspaces = workspaces xcf ++ ["lock"]
        , manageHook = manageLockWorkspace "lock" <+> manageHook xcf
        }
  where
    lockKeys :: XConfig l -> [((ButtonMask, KeySym), X ())]
    lockKeys XConfig {modMask = m} = [((m .|. shiftMask, xK_z), lock)]

-- Moves away new window from "lock" workspace regardless of current workspace
-- and focus.
manageLockWorkspace :: WorkspaceId -> ManageHook
manageLockWorkspace t   = ask >>= doF . lockWorkspace t

lockWorkspace :: WorkspaceId -> Window -> WindowSet -> WindowSet
lockWorkspace t w ws    = fromMaybe ws $ do
    i <- W.findTag w ws
    return $ if i == t
      then W.shiftWin (anotherWorkspace t ws) w ws
      else ws

-- Which workspace to choose, when new window has assigned to lock workspace.
anotherWorkspace :: WorkspaceId -> WindowSet -> WorkspaceId
anotherWorkspace t      = head . filter (/= t) . map W.tag . W.workspaces

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

