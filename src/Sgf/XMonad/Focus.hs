{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sgf.XMonad.Focus
    ( Focus
    , newWorkspace
    , focusedWindow
    , currentWorkspace
    , netActivated
    , FocusLock
    , toggleLock
    , FocusQuery
    , runFocusQuery
    , FocusHook

    -- Lifting into FocusQuery.
    , liftQuery
    , new
    , focused
    , focused'
    , focusedOn
    , focusedOn'
    , focusedCur
    , focusedCur'
    , newOn
    , newOnCur
    , activated

    -- Commonly used actions for modifying focus.
    , keepFocus
    , switchFocus
    , keepWorkspace
    , switchWorkspace

    -- Running FocusQuery.
    , manageFocus
    , manageActivate
    , activateEventHook
    , activateStartupHook
    , handleFocusQuery
    )
  where

import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Monad.Reader
import Control.Arrow hiding ((<+>))

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Hooks.ManageHelpers (currentWs)
import XMonad.Hooks.SetWMName

import Sgf.XMonad.X11


-- This module provides monad on top of Query monad providing additional
-- information about new window:
--  - workspace, where new window will appear;
--  - focused window on workspace, where new window will appear;
--  - current workspace;
--  - was new window _NET_ACTIVE_WINDOW activated or not (it will not be new
--  in that case, but i may work with it in the same way).
--
-- Lifting operations for standard ManageHook EDSL combinators into FocusQuery
-- monad allowing to run these combinators on focused window and common
-- actions for keeping focus and/or workspace, switching focus and/or
-- workspace are also provided.
--
-- To use this module with default FocusHook and `mod + v` for toggling focus
-- lock (when enabled, focus will not be switched to new window):
--
--      import XMonad
--
--      import Data.Tagged (witness)
--
--      import Sgf.XMonad.Config (SessionConfig)
--      import Sgf.XMonad.Focus
--
--      main :: IO ()
--      main = do
--              let xcf = handleFocusQuery (Just (0, xK_v)) (witness def (def :: SessionConfig a)) def
--              xmonad xcf
--
-- Note, that `handleFocusQuery` will enable window activation.
--
-- I may define my own FocusHook. For that i need more generic (-?>) and
-- `composeOne`, than ones defined in X.H.ManagerHelpers, though:
--
--      import Sgf.XMonad.Hooks.ManageHelpers
--
--      fh :: FocusHook
--      fh = composeOne
--              -- Always switch focus to `gmrun`.
--              [ new (className =? "Gmrun")
--                                      -?> switchFocus
--              -- If `gmrun` is focused on current workspace and new window
--              -- appears here too, keep focus unchanged.
--              , newOnCur <&&> focused (className =? "Gmrun")
--                                      -?> keepFocus
--              , activated -?> composeAll
--                  -- If `gmrun` is focused on workspace, on which
--                  -- activated window is, keep focus unchanged. I may
--                  -- still switch workspace there.
--                  [ focused (className =? "Gmrun")
--                                      --> keepFocus
--                  -- If firefox window is activated, do not switch
--                  -- workspace. I may still switch focus on that
--                  -- workspace.
--                  , new (className =? "Iceweasel" <||> className =? "Firefox")
--                                      --> keepWorkspace
--                  -- Default behavior for activated windows: switch
--                  -- workspace and focus.
--                  , return True       --> switchWorkspace <+> switchFocus
--                  ]
--              -- Default behavior for new windows: switch focus.
--              , return True           -?> switchFocus
--              ]
--
-- I may define my own `handleFocusQuery` too.
--
-- Note, that FocusHook will run *many* times, so it must not keep state or
-- save results. I.e. it must be idempotent to operate properly.
--
-- Note, that FocusHook will see new window at workspace, where functions on
-- the *right* from `handleFocusQuery` in ManageHook monoid place it.  In
-- other words, in `Endo WindowSet` monoid i may see changes only from
-- functions applied before (more to the right in function composition). Thus,
-- it's better to apply `handleFocusQuery` the last.
--
-- Moreover, FocusHook functions won't see window shift to another workspace
-- made by function from FocusHook itself: new window workspace is determined
-- *before* running FocusHook and even if later one of FocusHook functions
-- moves window to another workspace, predicates (`focused`, `newOn`, etc)
-- will still think new window is at workspace it was before. The reason is
-- how `manageFocus` works and can be fixed only by splitting FocusHook into
-- several different values and evaluating each one separately: make first
-- FocusHook, which may move window to another workspace, and evaluate it to
-- ManageHook by `manageFocus`. E.g. lock workspace implementation does this
-- for moving new windows out of lock workspace:
--
--      manageFocus def (newOn lockWs --> moveTo anotherWs)
--
--
-- Then make second FocusHook, which will (now) see results of first one, and
-- process further. E.g. regular `handleFocusQuery` should run after lock
-- workspace ManageHook, because the latter may change new window workspace:
--
--      handleFocusQuery .. <+> handleLock ..
--

data Focus          = Focus
                        -- Workspace, where new window appears.
                        { newWorkspace      :: WorkspaceId
                        -- Focused window on workspace, where new window
                        -- appears.
                        , focusedWindow     :: Maybe Window
                        -- Current workspace.
                        , currentWorkspace  :: WorkspaceId
                        -- Whether new window _NET_ACTIVE_WINDOW activated?
                        , netActivated      :: Bool
                        }
  deriving (Show)
instance Default Focus where
    def             = Focus
                        { focusedWindow     = Nothing
                        , newWorkspace      = ""
                        , currentWorkspace  = ""
                        , netActivated      = False
                        }

newtype FocusLock   = FocusLock Bool
  deriving (Show)
instance ExtensionClass FocusLock where
    initialValue    = FocusLock False

-- Toggle stored focus lock state.
toggleLock :: X ()
toggleLock          = XS.modify (\(FocusLock b) -> FocusLock (not b))

newtype FocusQuery a = FocusQuery (ReaderT Focus Query a)
instance Functor FocusQuery where
    fmap f (FocusQuery x) = FocusQuery (fmap f x)
instance Applicative FocusQuery where
    pure x                              = FocusQuery (pure x)
    (FocusQuery f) <*> (FocusQuery mx)  = FocusQuery (f <*> mx)
instance Monad FocusQuery where
    return x                = FocusQuery (return x)
    (FocusQuery mx) >>= f   = FocusQuery $ mx >>= \x ->
                              let FocusQuery y = f x in y
instance MonadReader Focus FocusQuery where
    ask                     = FocusQuery ask
    local f (FocusQuery mx) = FocusQuery (local f mx)
instance MonadIO FocusQuery where
    liftIO mx       = FocusQuery (liftIO mx)
instance Monoid a => Monoid (FocusQuery a) where
    mempty          = return mempty
    mappend         = liftM2 mappend

runFocusQuery :: FocusQuery a -> Focus -> Query a
runFocusQuery (FocusQuery m)    = runReaderT m

type FocusHook      = FocusQuery (Endo WindowSet)

-- Lifting into FocusQuery.
--
-- Lift Query into FocusQuery monad.
liftQuery :: Query a -> FocusQuery a
liftQuery           = FocusQuery . lift

-- Run Query on new window.
new :: Query a -> FocusQuery a
new                 = liftQuery

-- Run Query on focused window on workspace, where new window appears. If
-- there is no focused window, return False.
focused :: Query Bool -> FocusQuery Bool
focused m           = getAny <$> focused' (Any <$> m)
focused' :: Monoid a => Query a -> FocusQuery a
focused' m          = do
    mw <- asks focusedWindow
    liftQuery (maybe mempty (flip local m . const) mw)

-- Run Query on window focused at particular workspace. If there is no focused
-- window, return False.
focusedOn :: WorkspaceId -> Query Bool -> FocusQuery Bool
focusedOn i m       = getAny <$> focusedOn' i (Any <$> m)
focusedOn' :: Monoid a => WorkspaceId -> Query a -> FocusQuery a
focusedOn' i m      = liftQuery $ do
    mw <- liftX $ withWindowSet (return . W.peek . W.view i)
    maybe mempty (flip local m . const) mw

-- Run Query on focused window on current workspace. If there is no focused
-- window, return False.  Note, `focused <&&> newOnCur != focusedCur` . The
-- first will affect only new or activated window appearing on current
-- workspace, while the last will affect any window: focus even for windows
-- appearing on other workpsaces will depend on focus on *current* workspace.
focusedCur :: Query Bool -> FocusQuery Bool
focusedCur m        = getAny <$> focusedCur' (Any <$> m)
focusedCur' :: Monoid a => Query a -> FocusQuery a
focusedCur' m       = asks currentWorkspace >>= \i -> focusedOn' i m

-- Does new window appear at particular workspace?
newOn :: WorkspaceId -> FocusQuery Bool
newOn i             = (i ==) <$> asks newWorkspace
newOnCur :: FocusQuery Bool
newOnCur            = asks currentWorkspace >>= newOn

-- Does new window  _NET_ACTIVE_WINDOW activated?
activated :: FocusQuery Bool
activated           = asks netActivated

-- I don't know on which workspace new window will appear until i actually run
-- (Endo WindowSet) function (in `windows` in XMonad.Operations), but in (Endo
-- WindowSet) function i can't already execute monadic actions, because it's
-- pure. So, i compute result for every workspace here and just use it later
-- in (Endo WindowSet) function.  Note, though, that this will execute monadic
-- actions many times, and therefore assume, that result of FocusHook does
-- not depend on the number of times it was executed.
manageFocus :: Focus -> FocusHook -> ManageHook
manageFocus r m     = do
    fws <- liftX . withWindowSet $ return
      . map (W.tag &&& fmap W.focus . W.stack) . W.workspaces
    ct  <- currentWs
    let r' = r {currentWorkspace = ct}
    hs <- forM fws $ \(i, mw) -> do
      f <- runFocusQuery m (r' {focusedWindow = mw, newWorkspace = i})
      return (i, f)
    reader (selectHook hs) >>= doF
  where
    -- Select and apply (Endo WindowSet) function depending on which workspace
    -- new window appeared now.
    selectHook :: [(WorkspaceId, Endo WindowSet)] -> Window -> WindowSet -> WindowSet
    selectHook cfs nw ws    = fromMaybe ws $ do
        i <- W.findTag nw ws
        f <- lookup i cfs
        return (appEndo f ws)

manageActivate :: FocusHook -> ManageHook
manageActivate      = manageFocus (def {netActivated = True})

-- Commonly used actions for modifying focus.
--
-- Note, that pair of operations `keepFocus` and `switchFocus`,
-- `keepWorkspace` and `switchWorkspace` negate each other and are commutative
-- in FocusQuery monoid.
--
-- Keep focus on workspace (may not be current), where new window appears.
-- Workspace will not be switched. This operation is idempotent and
-- effectively returns focus to window focused on that workspace before
-- applying (Endo WindowSet) function.
keepFocus :: FocusHook
keepFocus           = focused' $ ask >>= \w -> doF $ \ws ->
                        W.view (W.currentTag ws) . W.focusWindow w $ ws

-- Switch focus to new window on workspace (may not be current), where new
-- window appears. Workspace will not be switched. This operation is
-- idempotent. When focus lock is enabled, i explicitly call `keepFocus`
-- (still no `keepWorkspace`) to overwrite default behavior.
switchFocus :: FocusHook
switchFocus         = do
    FocusLock b <- liftQuery . liftX $ XS.get
    if b
      then keepFocus
      else new $ ask >>= \w -> doF $ \ws ->
            W.view (W.currentTag ws) . W.focusWindow w $ ws

-- Keep current workspace. Focus will not be changed at either current or new
-- window's  workspace. This operation is idempotent and effectively switches
-- to workspace, which was current before applying (Endo WindowSet) function.
keepWorkspace :: FocusHook
keepWorkspace       = do
    ws <- asks currentWorkspace
    liftQuery . doF $ W.view ws

-- Switch workspace to one, where new window appears. Focus will not be
-- changed at either current or new window's workspace. This operation is
-- idempotent. When focus lock is enabled i explicitly call `keepWorkspace`
-- (still no `keepFocus`) to overwrite default behavior.
switchWorkspace :: FocusHook
switchWorkspace     = do
    FocusLock b <- liftQuery . liftX $ XS.get
    if b
      then keepWorkspace
      else do
        ws <- asks newWorkspace
        liftQuery . doF $ W.view ws

activateEventHook :: FocusHook -> Event -> X All
activateEventHook x ClientMessageEvent {
                    ev_window = w,
                    ev_message_type = mt
                }   = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    when (mt == a_aw) $ runQuery (manageActivate x) w >>= windows . appEndo
    return (All True)
activateEventHook _ _   = return (All True)

handleFocusQuery :: Maybe (ButtonMask, KeySym)  -- Key to toggle focus lock.
                    -> FocusHook
                    -> XConfig l -> XConfig l
handleFocusQuery ml x cf   = (additionalKeys <*> addLockKey ml) $ cf
    { manageHook        = manageFocus def x `mappend` manageHook cf
    , startupHook       = startupHook cf >> activateStartupHook
    , handleEventHook   = activateEventHook x `mappend` handleEventHook cf
    }
  where
    addLockKey :: Maybe (ButtonMask, KeySym) -> XConfig l
                      -> [((ButtonMask, KeySym), X ())]
    addLockKey (Just (mk, k)) XConfig{modMask = m} =
                            [((m .|. mk, k), toggleLock)]
    addLockKey Nothing _ =  []

-- `setWMName` creates support window (don't know why), sets its _NET_WM_NAME
-- to specified value, sets '_NET_SUPPORTING_WM_CHECK' atom of support window
-- and root window to support window id and and adds two atoms
-- '_NET_SUPPORTING_WM_CHECK' and '_NET_WM_NAME' to '_NET_SUPPORTED' atom of
-- root window (removing any duplicates). And this is required (apart from
-- adding '_NET_ACTIVE_WINDOW' to '_NET_SUPPORTED') for making
-- window activation work. Also, `setWMName` checks window pointed by
-- '_NET_SUPPORTING_WM_CHECK' before creating support window, so it's safe to
-- call it many times - only window name in '_NET_WM_NAME' may change.
activateStartupHook :: X ()
activateStartupHook = do
                        setWMName "xmonad"
                        getAtom "_NET_ACTIVE_WINDOW" >>= addNETSupported

