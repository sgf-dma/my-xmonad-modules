{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sgf.XMonad.Focus
    ( Focus
    , newWorkspace
    , focusedWindow
    , currentWorkspace
    , NetActivated
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
import XMonad.Hooks.ManageHelpers (currentWs)
import XMonad.Hooks.SetWMName

import Sgf.XMonad.X11
import Sgf.XMonad.Util.EZConfig


-- This module provides monad on top of Query monad providing additional
-- information about new window:
--  - workspace, where new window will appear;
--  - focused window on workspace, where new window will appear;
--  - current workspace;
-- And two properties in extensible state:
--  - is focus lock enabled? Focus lock instructs all library's FocusHook
--  functions to not move focus.
--  - is new window _NET_ACTIVE_WINDOW activated? It is not really new in that
--  case, but i may work with it in the same way.
--
-- Lifting operations for standard ManageHook EDSL combinators into FocusQuery
-- monad allowing to run these combinators on focused window and common
-- actions for keeping focus and/or workspace, switching focus and/or
-- workspace are also provided.
--
-- WARNING! `activateEventHook` (which handles window activation) will use
-- `manageHook` for handling activated window. That means, that actions, which
-- you don't want to happen on activated windows, should be guarded by
-- `not <$> activated` predicate (this, consequently, requires to lift them
-- into `FocusHook` and then convert to `ManageHook` back using
-- `manageFocus`).
--
-- WARNING! Since this module enables and handles window activation on its
-- own, it is *not* compatible with `ewmh` function from
-- 'XMonad.Hooks.EwmhDesktops' module. Well, it will compile and work, but
-- window activation handling according to `FocusHook` won't work, because
-- `ewmh` handler will overwrite it.
--
-- To use this module with default FocusHook and `mod + v` for toggling focus
-- lock (when enabled, focus will not be switched to new window):
--
--      import XMonad
--
--      import Sgf.XMonad.Config (SessionConfig(..))
--      import Sgf.XMonad.Focus
--
--      main :: IO ()
--      main = do
--              let xcf = handleFocusQuery (Just (0, xK_v)) (composeOne
--                              [ activated -?> (activateFocusHook def)
--                              , Just <$> (newFocusHook def)
--                              ])
--                          $ def
--              xmonad xcf
--
--      composeOne :: (Monoid a, Monad m) => [m (Maybe a)] -> m a
--      composeOne [] = return mempty
--      composeOne (mx : xs) = do
--          x <- mx
--          case x of
--            Just y  -> return y
--            Nothing -> composeOne xs
--
--      infixr 0 -?>
--      (-?>) :: Monad m => m Bool -> m a -> m (Maybe a)
--      (-?>) mb mx     = do
--          b <- mb
--          if b
--            then Just <$> mx
--            else return Nothing
--
-- Note:
--  - `handleFocusQuery` will enable window activation;
--  - i shouldn't specify modMask in lock focus key definition:
--  `handleFocusQuery` will add modMask automatically;
--  - i need more generic (-?>) and `composeOne`, than in the
--  'XMonad.Hooks.ManageHelpers'.
--  - the order, when constructing final FocusHook in `handleFocusQuery` call:
--  FocusHook without `activated` predicate will match to activated windows
--  too, thus i should place it after one with `activated` (so it will have a
--  chance to handle activated window first).
--
-- I may define my own FocusHook too:
--
--      activateFocusHook :: FocusHook
--      activateFocusHook = composeAll
--                      -- If `gmrun` is focused on workspace, on which
--                      -- activated window is, keep focus unchanged. But i
--                      -- may still switch workspace.
--                      [ focused (className =? "Gmrun")
--                                      --> keepFocus
--                      -- Default behavior for activated windows: switch
--                      -- workspace and focus.
--                      , return True   --> switchWorkspace <+> switchFocus
--                      ]
--
--      newFocusHook :: FocusHook
--      newFocusHook      = composeOne
--                      -- Always switch focus to `gmrun`.
--                      [ new (className =? "Gmrun")        -?> switchFocus
--                      -- And always keep focus on `gmrun`. Note, that
--                      -- another `gmrun` will steal focus from already
--                      -- running one.
--                      , focused (className =? "Gmrun")    -?> keepFocus
--                      -- If firefox dialog prompt (e.g. master password
--                      -- prompt) is focused on current workspace and new
--                      -- window appears here too, keep focus unchanged
--                      -- (note, used predicates: `newOnCur <&&> focused` is
--                      -- the same as `newOnCur <&&> focusedCur`, but is
--                      -- *not* the same as just `focusedCur` )
--                      , newOnCur <&&> focused
--                          ((className =? "Iceweasel" <||> className =? "Firefox") <&&> isDialog)
--                                                          -?> keepFocus
--                      -- Default behavior for new windows: switch focus.
--                      , return True                       -?> switchFocus
--                      ]
--
-- Some more technical notes:
--  - FocusHook will run *many* times, so it usually should not keep state or
--  save results. Particularly, it may do anything, but it must be idempotent
--  to operate properly.
--  - FocusHook will see new window at workspace, where functions on the
--  *right* from `handleFocusQuery` in ManageHook monoid place it.  In other
--  words, in `Endo WindowSet` monoid i may see changes only from functions
--  applied before (more to the right in function composition). Thus, it's
--  better to apply `handleFocusQuery` the last.
--  - FocusHook functions won't see window shift to another workspace made by
--  function from FocusHook itself: new window workspace is determined
--  *before* running FocusHook and even if later one of FocusHook functions
--  moves window to another workspace, predicates (`focused`, `newOn`, etc)
--  will still think new window is at workspace it was before.  This can be
--  worked around by splitting FocusHook into several different values and
--  evaluating each one separately, like:
--
--      (FH2 -- manageFocus --> MH2) <+> (FH1 -- manageFocus --> MH1) <+> ..
--
--  E.g.
--
--      manageFocus FH2 <+> manageFocus FH1 <+> ..
--
--  now FH2 will see window shift made by FH1.
--
--  - I may define my own `handleFocusQuery` too, all required functions are
--  exported. I may redefine handling of activated windows too, but note:
--  `handleEventHook` handling window activation should correctly set/unset
--  `NetActivated` in extensible state, like `activateEventHook` does, and
--  usually there should be only one `handleEventHook` processing activated
--  windows.
--
-- Finally, another interesting example is moving activated window to current
-- workspace by default, while still applying FocusHook:
--
--      import XMonad
--      import qualified XMonad.StackSet as W
--
--      import Sgf.XMonad.Config (SessionConfig (..))
--      import Sgf.XMonad.Focus
--
--      main :: IO ()
--      main = do
--              let xcf = handleFocusQuery (Just (0, xK_v)) (composeOne
--                              [ activated -?> (newOnCur --> keepFocus) <+> (activateFocusHook def)
--                              , Just <$> (newFocusHook def)
--                              ])
--                          $ def
--                              { modMask = mod4Mask
--                              , manageHook = manageFocus (activated -->
--                                              (asks currentWorkspace >>= \i ->
--                                               new (reader (W.shiftWin i) >>= doF)))
--                              }
--              xmonad xcf
--
-- Here i want to keep focus, if activated window appears on current workspace
-- (beyond what is already done by default activated window FocusHook).

data Focus          = Focus
                        -- Workspace, where new window appears.
                        { newWorkspace      :: WorkspaceId
                        -- Focused window on workspace, where new window
                        -- appears.
                        , focusedWindow     :: Maybe Window
                        -- Current workspace.
                        , currentWorkspace  :: WorkspaceId
                        }
  deriving (Show)
instance Default Focus where
    def             = Focus
                        { focusedWindow     = Nothing
                        , newWorkspace      = ""
                        , currentWorkspace  = ""
                        }

newtype FocusLock   = FocusLock Bool
  deriving (Show)
instance ExtensionClass FocusLock where
    initialValue    = FocusLock False

-- Toggle stored focus lock state.
toggleLock :: X ()
toggleLock          = XS.modify (\(FocusLock b) -> FocusLock (not b))

-- Whether new window _NET_ACTIVE_WINDOW activated or not. I should keep this
-- value in global state, because i use `ManageHook` for handling activated
-- windows and i need a way to tell `manageHook`, that now a window is
-- activated.
newtype NetActivated    = NetActivated {netActivated :: Bool}
  deriving (Show)
instance ExtensionClass NetActivated where
    initialValue        = NetActivated False

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
activated           = liftQuery (liftX XS.get) >>= return . netActivated

-- I don't know on which workspace new window will appear until i actually run
-- (Endo WindowSet) function (in `windows` in XMonad.Operations), but in (Endo
-- WindowSet) function i can't already execute monadic actions, because it's
-- pure. So, i compute result for every workspace here and just use it later
-- in (Endo WindowSet) function.  Note, though, that this will execute monadic
-- actions many times, and therefore assume, that result of FocusHook does
-- not depend on the number of times it was executed.
manageFocus :: FocusHook -> ManageHook
manageFocus m       = do
    fws <- liftX . withWindowSet $ return
      . map (W.tag &&& fmap W.focus . W.stack) . W.workspaces
    ct  <- currentWs
    let r = def {currentWorkspace = ct}
    hs <- forM fws $ \(i, mw) -> do
      f <- runFocusQuery m (r {focusedWindow = mw, newWorkspace = i})
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

activateEventHook :: ManageHook -> Event -> X All
activateEventHook x ClientMessageEvent {
                    ev_window = w,
                    ev_message_type = mt
                }   = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    -- `NetActivated` state handling is done solely and completely here!
    when (mt == a_aw) $ do
      XS.put (NetActivated True)
      runQuery x w >>= windows . appEndo
      XS.put (NetActivated False)
    return (All True)
activateEventHook _ _   = return (All True)

handleFocusQuery :: Maybe (ButtonMask, KeySym)  -- Key to toggle focus lock.
                    -> FocusHook
                    -> XConfig l -> XConfig l
handleFocusQuery mt x cf = addLockKey $ cf
    -- Note, the order: i want to apply FocusHook after user's changes, which
    -- may change new/activated window workspace. Thus, in `manageHook`, which
    -- is function composition, i should add in Monoid to the left, but in
    -- `handleEventHook`, which runs actions from left to right, to the right!
    { manageHook        = mh
    , handleEventHook   = handleEventHook cf `mappend` activateEventHook mh
    -- Note, the order: i make my changes after user's changes here too.
    , startupHook       = startupHook cf >> activateStartupHook
    }
  where
    -- Note, `manageHook` should *not* touch `NetActivated` state value at
    -- all!  Because `manageHook` may be called either on its own (from
    -- `manage` in X.Operations.hs) or from `activateEventHook` (from here),
    -- the only one who knows was window activated or not is the caller. And
    -- it should set and unset `NetActivated` state properly.  Here this is
    -- done solely and completely by `activateEventHook`.
    mh :: ManageHook
    mh              = manageFocus x `mappend` manageHook cf
    addLockKey :: XConfig l -> XConfig l
    addLockKey      = additionalKeys' <*> mt `maybeKey` toggleLock

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

