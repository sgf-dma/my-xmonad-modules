
module Sgf.XMonad.Util.EZConfig
    ( mapKeys
    , cloneKeys
    , addModMask
    , maybeAddModMask
    , maybeKey
    , additionalKeys'
    , appendKeys
    )
  where

import Data.Maybe
import Control.Arrow
import qualified Data.Map as M

import XMonad
import XMonad.Util.EZConfig

import Sgf.Control.Lens
import Sgf.Control.Applicative


-- Map function over keys's Map .
mapKeys :: (M.Map (ButtonMask, KeySym) (X ()) -> M.Map (ButtonMask, KeySym) (X ()))
           -> XConfig a -> XConfig a
mapKeys f conf      = conf {keys = fmap f (keys conf)}

-- Define each key second time with different modMask (obtained by passing
-- current modMask to function). Suitable for use with mapKeys .
cloneKeys :: (ButtonMask -> ButtonMask)
                     -> M.Map (ButtonMask, KeySym) (X ())
                     -> M.Map (ButtonMask, KeySym) (X ())
cloneKeys f xs      = xs `M.union` M.mapKeys (first f) xs

-- Helper function for use in `additinalKeys <*> mt `maybeKey` x` , where
-- `mt :: Maybe (ButtonMask, KeySym)` and `x :: X ()` .
maybeKey :: Maybe (ButtonMask, KeySym) -> X () -> XConfig l -> [((ButtonMask, KeySym), X ())]
maybeKey mk x       = pure . maybeToList $ (mk >>= \k -> return (k, x))

addModMask :: XConfig l -> (ButtonMask, KeySym) -> (ButtonMask, KeySym)
addModMask XConfig{modMask = m}   = first (.|. m)

maybeAddModMask :: Maybe (ButtonMask, KeySym) -> XConfig l -> Maybe (ButtonMask, KeySym)
maybeAddModMask     = modifyAA maybeL (\x -> addModMask <*> pure x)

-- Variant of additionalKeys, which adds modMask to key's ButtonMask,
additionalKeys' :: XConfig a -> [((ButtonMask, KeySym), X ())] -> XConfig a
additionalKeys'     = additionalKeys <.> (map . first <$> addModMask)

-- Variant of additionalKeys, which appends new key action to the existing one
-- (if any) instead of overwriting it. That means, that single key will
-- executing both old and new actions.
appendKeys :: XConfig a -> [((ButtonMask, KeySym), X ())] -> XConfig a
appendKeys xcf ys   = xcf {keys = M.unionWith (>>) (M.fromList ys) . keys xcf}

