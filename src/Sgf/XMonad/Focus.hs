{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Sgf.XMonad.Focus
    ( onFocused
    , FocusHook
    , focusedWindow
    , newWindow
    , lockFocus
    , defaultFocusHook
    , handleFocus
    )
  where

import Data.Maybe
import Data.Monoid
import Control.Applicative

import XMonad
import qualified XMonad.StackSet as W
import qualified XMonad.Util.ExtensibleState as XS
import XMonad.Util.EZConfig (additionalKeys)

import Sgf.Control.Lens


-- Helper for ManageHook: run Query on focused window (instead of new window),
-- and lift result into Query again (if there is no windows on current
-- workspace, return default result).
onFocused :: a -> Query a -> Query a
onFocused b m       = liftX $
    withWindowSet (maybe (return b) (runQuery m) . W.peek)

-- Should window focus be kept on current window or switched to new one:
-- focusedWindow ManageHook will run on focused window and, if matched (True),
-- will keep focus still. newWindow ManageHook will run on new window and, if
-- matched, will overwrite focusedWindow result. lockFocus will overwrite
-- anything and just keep focus unchanged (Nothing means ignore, Just True -
-- keep focus, Just False - change to new window).
data FocusHook      = FocusHook
                        { _focusedWindow    :: Query Bool
                        , _newWindow        :: Query Bool
                        , _lockFocus        :: Last  Bool
                        }
  deriving (Typeable)
focusedWindow :: LensA FocusHook (Query Bool)
focusedWindow f z@(FocusHook {_focusedWindow = x})
                    = fmap (\x' -> z{_focusedWindow = x'}) (f x)
newWindow :: LensA FocusHook (Query Bool)
newWindow f z@(FocusHook {_newWindow = x})
                    = fmap (\x' -> z{_newWindow = x'}) (f x)
lockFocus :: LensA FocusHook (Maybe Bool)
lockFocus           = lockFocus' . lastL
lockFocus' :: LensA FocusHook (Last Bool)
lockFocus' f z@(FocusHook {_lockFocus = x})
                    = fmap (\x' -> z{_lockFocus = x'}) (f x)
defaultFocusHook :: FocusHook
defaultFocusHook    = FocusHook
                        { _focusedWindow    = return False
                        , _newWindow        = return False
                        , _lockFocus        = Last Nothing
                        }

instance ExtensionClass FocusHook where
    initialValue    = setA lockFocus (Just False) defaultFocusHook
-- Note, that i can't set lockFocus to Nothing using `mappend` once it is set
-- to Just.
instance Monoid FocusHook where
    mempty          = defaultFocusHook
    x `mappend` y   = modifyA focusedWindow (<||> viewA focusedWindow y)
                        . modifyA newWindow (<||> viewA newWindow y)
                        . modifyA lockFocus' (`mappend` viewA lockFocus' y)
                        $ x

-- Handle focus changes and add key for toggling focus lock. When handleFocus
-- tries to keep focus still, it needs to know where new window will appear:
-- on current workspace or not. But it can detect window shifts only performed
-- in ManageHooks before its own ManageHook (manageFocus). Thus, `handleFocus`
-- should be the last function applied to XConfig to avoid incorrect focus
-- changes.
handleFocus :: LayoutClass l Window => Maybe (ButtonMask, KeySym)
               -> [FocusHook] -> XConfig l -> XConfig l
handleFocus ml ps cf    = (additionalKeys <*> addLockKey ml) $ cf
    { manageHook    = manageFocus <+> manageHook cf
    , startupHook   = mapM_ addFocusHook ps >> startupHook cf
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

-- ManageHook for switching (or not) focus.
manageFocus :: ManageHook
manageFocus         = do
    x <- liftX XS.get
    let pf = viewA focusedWindow x
        pn = viewA newWindow x
        b  = fromMaybe False (viewA lockFocus x)
    return b <||> ((not <$> pn) <&&> onFocused False pf) -->
      ask >>= doF . focusDown

-- Toggle stored focus lock state.
toggleLock :: X ()
toggleLock      = XS.modify (modifyA lockFocus (not <$>))

