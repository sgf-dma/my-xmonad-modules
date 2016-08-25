{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Sgf.XMonad.Focus
    ( onFocused
    , FocusHook
    , focusedWindowOld
    , newWindow
    , activateWindow
    , focusLock
    , defaultFocusHook
    , handleFocus
    , focusWindow
    , shiftWindow

    , Focus
    , newWorkspace
    , focusedWindow
    , currentWorkspace
    , netActivated
    , FocusQuery
    , runFocusQuery
    , FocusHook7
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
    , keepFocus
    , switchFocus
    , keepWorkspace
    , switchWorkspace
    , handleFocusQuery
    )
  where

import Data.Maybe
import Data.Monoid
import Control.Monad
import Control.Monad.Reader
-- FIXME: Hide <+> in XMonad instead!
import Control.Arrow hiding ((<+>))

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Hooks.ManageHelpers (currentWs)

import Sgf.Control.Lens


-- FIXME: Use local?
-- Helper for ManageHook: run Query on focused window (instead of new window),
-- and lift result into Query again (if there is no windows on current
-- workspace, return default result).
onFocused :: a -> Query a -> Query a
onFocused b m       = liftX $
    withWindowSet (maybe (return b) (runQuery m) . W.peek)


data Focus          = Focus
                        -- Workspace, where new window appears.
                        { _newWorkspace     :: WorkspaceId
                        -- Focused window on workspace, where new window
                        -- appears.
                        , _focusedWindow    :: Maybe Window
                        -- Current workspace.
                        , _currentWorkspace :: WorkspaceId
                        -- Whether new window _NET_ACTIVE_WINDOW activated?
                        , _netActivated     :: Bool
                        }
  deriving (Show)
newWorkspace :: LensA Focus WorkspaceId
newWorkspace f z@Focus {_newWorkspace = x}
                    = fmap (\x' -> z{_newWorkspace = x'}) (f x)
focusedWindow :: LensA Focus (Maybe Window)
focusedWindow f z@Focus {_focusedWindow = x}
                    = fmap (\x' -> z{_focusedWindow = x'}) (f x)
currentWorkspace :: LensA Focus WorkspaceId
currentWorkspace f z@Focus {_currentWorkspace = x}
                    = fmap (\x' -> z{_currentWorkspace = x'}) (f x)
netActivated :: LensA Focus Bool
netActivated f z@Focus {_netActivated = x}
                    = fmap (\x' -> z{_netActivated = x'}) (f x)
instance Default Focus where
    def             = Focus
                        { _focusedWindow    = Nothing
                        , _newWorkspace     = ""
                        , _currentWorkspace = ""
                        , _netActivated     = False
                        }

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

type FocusHook7     = FocusQuery (Endo WindowSet)

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
focused m           = do
    mw <- asks (viewA focusedWindow)
    liftQuery (maybe (return False) (flip local m . const) mw)
focused' :: Monoid a => Query a -> FocusQuery a
focused' m          = do
    mw <- asks (viewA focusedWindow)
    liftQuery (maybe mempty (flip local m . const) mw)

-- Run Query on window focused at particular workspace. If there is no focused
-- window, return False.
focusedOn :: WorkspaceId -> Query Bool -> FocusQuery Bool
focusedOn i m       = liftQuery $ do
    mw <- liftX $ withWindowSet (return . W.peek . W.view i)
    maybe (return False) (flip local m . const) mw
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
focusedCur m        = liftQuery currentWs >>= \i -> focusedOn i m
focusedCur' :: Monoid a => Query a -> FocusQuery a
focusedCur' m       = liftQuery currentWs >>= \i -> focusedOn' i m

-- Does new window appear at particular workspace?
newOn :: WorkspaceId -> FocusQuery Bool
newOn i             = (i ==) <$> asks (viewA newWorkspace)
newOnCur :: FocusQuery Bool
newOnCur            = asks (viewA currentWorkspace) >>= newOn

-- Does new window  _NET_ACTIVE_WINDOW activated?
activated :: FocusQuery Bool
activated           = asks (viewA netActivated)

-- I don't know on which workspace new window will appear until i actually run
-- (Endo WindowSet) function (in `windows` in XMonad.Operations), but in (Endo
-- WindowSet) function i can't already execute monadic actions, because it's
-- pure. So, i compute result for every workspace here and just use it later
-- in (Endo WindowSet) function.  Note, though, that this will execute monadic
-- actions many times, and therefore assume, that result of FocusHook7 does
-- not depend on the number of times it was executed.
manageFocus :: Focus -> FocusHook7 -> ManageHook
manageFocus r m     = do
    fws <- liftX . withWindowSet $ return
      . map (W.tag &&& fmap W.focus . W.stack) . W.workspaces
    ct  <- currentWs
    let r' = setA currentWorkspace ct r
    hs <- forM fws $ \(i, mw) -> do
      f <- runFocusQuery m (setA focusedWindow mw . setA newWorkspace i $ r')
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

manageActivate :: FocusHook7 -> ManageHook
manageActivate      = manageFocus (setA netActivated True def)

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
keepFocus :: FocusHook7
keepFocus           = focused' $ ask >>= \w -> doF $ \ws ->
                        W.view (W.currentTag ws) . W.focusWindow w $ ws

-- Switch focus to new window on workspace (may not be current), where new
-- window appears. Workspace will not be switched. This operation is
-- idempotent. When focus lock is enabled, i explicitly call `keepFocus`
-- (still no `keepWorkspace`) to overwrite default behavior.
switchFocus :: FocusHook7
switchFocus         = do
    x <- liftQuery . liftX $ XS.get
    let b = fromMaybe False (viewA focusLock x)
    if b
      then keepFocus
      else new $ ask >>= \w -> doF $ \ws ->
            W.view (W.currentTag ws) . W.focusWindow w $ ws

-- Keep current workspace. Focus will not be changed at either current or new
-- window's  workspace. This operation is idempotent and effectively switches
-- to workspace, which was current before applying (Endo WindowSet) function.
keepWorkspace :: FocusHook7
keepWorkspace       = do
    ws <- asks (viewA currentWorkspace)
    liftQuery . doF $ W.view ws

-- Switch workspace to one, where new window appears. Focus will not be
-- changed at either current or new window's workspace. This operation is
-- idempotent. When focus lock is enabled i explicitly call `keepWorkspace`
-- (still no `keepFocus`) to overwrite default behavior.
switchWorkspace :: FocusHook7
switchWorkspace     = do
    x <- liftQuery . liftX $ XS.get
    let b = fromMaybe False (viewA focusLock x)
    if b
      then keepWorkspace
      else do
        ws <- asks (viewA newWorkspace)
        liftQuery . doF $ W.view ws

activateEventHook :: FocusHook7 -> Event -> X All
activateEventHook x ClientMessageEvent {
                    ev_window = w,
                    ev_message_type = mt
                }   = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    when (mt == a_aw) $ runQuery (manageActivate x) w >>= windows . appEndo
    return (All True)
activateEventHook _ _   = return (All True)

handleFocusQuery :: Maybe (ButtonMask, KeySym) -> FocusHook7 -> XConfig l -> XConfig l
handleFocusQuery ml x cf   = (additionalKeys <*> addLockKey ml) $ cf
    { manageHook        = manageFocus def x `mappend` manageHook cf
    , startupHook       = do
                            startupHook cf
                            addWMActivateSupport
    , handleEventHook   = activateEventHook x `mappend` handleEventHook cf
    }
  where
    addLockKey :: Maybe (ButtonMask, KeySym) -> XConfig l
                      -> [((ButtonMask, KeySym), X ())]
    addLockKey (Just (mk, k)) XConfig{modMask = m} =
                            [((m .|. mk, k), toggleLock)]
    addLockKey Nothing _ =  []


-- Should window focus be kept on current window or switched to new one:
-- focusedWindow ManageHook will run on focused window and, if matched (True),
-- will keep focus still. newWindow ManageHook will run on new window and, if
-- matched, will overwrite focusedWindow result and shift focus to new window.
-- focusLock will overwrite anything and just keep focus unchanged (Nothing
-- means ignore, Just True - keep focus, Just False - change to new window).
data FocusHook      = FocusHook
                        { _focusedWindowOld :: Query Bool
                        , _newWindow        :: Query Bool
                        , _activateWindow  :: ManageHook
                        , _focusLock        :: Last  Bool
                        }
  deriving (Typeable)
focusedWindowOld :: LensA FocusHook (Query Bool)
focusedWindowOld f z@FocusHook {_focusedWindowOld = x}
                    = fmap (\x' -> z{_focusedWindowOld = x'}) (f x)
newWindow :: LensA FocusHook (Query Bool)
newWindow f z@FocusHook {_newWindow = x}
                    = fmap (\x' -> z{_newWindow = x'}) (f x)
activateWindow :: LensA FocusHook ManageHook
activateWindow f z@FocusHook {_activateWindow = x}
                    = fmap (\x' -> z{_activateWindow = x'}) (f x)
focusLock :: LensA FocusHook (Maybe Bool)
focusLock           = focusLock' . lastL
focusLock' :: LensA FocusHook (Last Bool)
focusLock' f z@FocusHook {_focusLock = x}
                    = fmap (\x' -> z{_focusLock = x'}) (f x)
defaultFocusHook :: FocusHook
defaultFocusHook    = FocusHook
                        { _focusedWindowOld    = return False
                        , _newWindow        = return False
                        , _activateWindow  = return (Endo id)
                        , _focusLock        = Last Nothing
                        }

instance ExtensionClass FocusHook where
    initialValue    = setA focusLock (Just False) defaultFocusHook
-- Note, that i can't set focusLock to Nothing using `mappend` once it is set
-- to Just.
instance Monoid FocusHook where
    mempty          = defaultFocusHook
    x `mappend` y   = modifyA focusedWindowOld (<||> viewA focusedWindowOld y)
                        . modifyA activateWindow
                            (`mappend` viewA activateWindow y)
                        . modifyA newWindow (<||> viewA newWindow y)
                        . modifyA focusLock' (`mappend` viewA focusLock' y)
                        $ x

-- Handle focus changes and add key for toggling focus lock. When handleFocus
-- tries to keep focus still, it needs to know where new window will appear:
-- on current workspace or not. But it can detect window shifts only performed
-- in ManageHooks before its own ManageHook (manageFocusOld). Thus, `handleFocus`
-- should be the last function applied to XConfig to avoid incorrect focus
-- changes.
handleFocus :: Maybe (ButtonMask, KeySym)
               -> [FocusHook] -> XConfig l -> XConfig l
handleFocus ml ps cf    = (additionalKeys <*> addLockKey ml) $ cf
    { manageHook    = manageFocusOld <+> manageHook cf
    , startupHook   = do
                        mapM_ addFocusHook ps
                        startupHook cf
                        addWMActivateSupport
    , handleEventHook = activateEventHookOld <+> handleEventHook cf
    }
  where
    addLockKey :: Maybe (ButtonMask, KeySym) -> XConfig l
                      -> [((ButtonMask, KeySym), X ())]
    addLockKey (Just (mk, k)) XConfig{modMask = m} =
                            [((m .|. mk, k), toggleLock)]
    addLockKey Nothing _ =  []

-- Add new FocusHook to current stored FocusHook value.
addFocusHook :: FocusHook -> X ()
addFocusHook x      = XS.modify (x `mappend`)

-- Move focus down only, if specified window is on current workspace. This is
-- main ManageHook part for keeping focus still: if new window appeared on
-- current workspace, i need to move focus down; otherwise, if e.g. previous
-- ManageHook functions move new window to another workspace, i don't need to
-- shift focus. But for this to work, this function should be applied last in
-- ManageHook's function composition (Endo monoid).
focusDown :: Window -> WindowSet -> WindowSet
focusDown w ws
  | W.findTag w ws == Just cw   = W.focusDown ws
  | otherwise                   = ws
  where cw = W.currentTag ws

-- Search for workspace, where new window appears, and set focus there to
-- remembered previously focused window (if it is still there).
keepFocusOld :: [(WorkspaceId, Maybe Window)] -> Window -> WindowSet -> WindowSet
keepFocusOld cfs nw ws = fromMaybe (W.view (W.currentTag ws) . W.focusWindow nw $ ws) $ do
    i  <- W.findTag nw ws
    cw <- join (lookup i cfs)
    j  <- W.findTag cw ws
    if i == j
      then return (W.view (W.currentTag ws) . W.focusWindow cw $ ws)
      else mzero

-- ManageHook's pure function is computed in function 'manage` in
-- XMonad/Operations.hs in still unmodified WindowSet (new window will be
-- added later by default pure function, when it is applied by `windows`).
-- Thus, i remember focused window for each workspace here and use this
-- information in pure `keepFocusOld` function to preserve focus on workspace,
-- where new window will appear (any workspace, not only current).
keepFocusHook :: ManageHook
keepFocusHook       = do
    cfs <- liftX . withWindowSet $ return
      . map (\w -> (W.tag w, W.focus <$> W.stack w))
      . W.workspaces
    ask >>= doF . keepFocusOld cfs

-- ManageHook for switching (or not) focus.
manageFocusOld :: ManageHook
manageFocusOld         = do
    x <- liftX XS.get
    let pf = viewA focusedWindowOld x
        pn = viewA newWindow x
        b  = fromMaybe False (viewA focusLock x)
    return b <||> ((not <$> pn) <&&> onFocused False pf) --> keepFocusHook

manageActivateOld :: ManageHook
manageActivateOld      = do
    x <- liftX XS.get
    let pf = viewA focusedWindowOld x
        pa = viewA activateWindow x
        b  = fromMaybe False (viewA focusLock x)
    return (not b) <&&> (not <$> onFocused False pf) --> pa

activateEventHookOld :: Event -> X All
activateEventHookOld ClientMessageEvent {
                    ev_window = w,
                    ev_message_type = mt
                }   = do
    a_aw <- getAtom "_NET_ACTIVE_WINDOW"
    when (mt == a_aw) $ runQuery manageActivateOld w >>= windows . appEndo
    return (All True)
activateEventHookOld _   = return (All True)

addWMActivateSupport :: X ()
addWMActivateSupport  = withDisplay $ \dpy -> do
    r <- asks theRoot
    a <- getAtom "_NET_SUPPORTED"
    c <- getAtom "ATOM"
    supp <- getAtom "_NET_ACTIVE_WINDOW"
    io $ changeProperty32 dpy r a c propModeAppend [fromIntegral supp]

-- Toggle stored focus lock state.
toggleLock :: X ()
toggleLock      = XS.modify (modifyA focusLock (not <$>))

focusWindow :: ManageHook
focusWindow         = ask >>= doF . W.focusWindow

shiftWindow :: ManageHook
--shiftWindow i       = ask >>= doF . W.shiftWin i
shiftWindow         = ask >>= doF . (\w s -> W.shiftWin (W.currentTag s) w s)

