
module Sgf.XMonad.Util.EZConfig
    ( mapKeys
    , addModMask
    , additionalKeys'
    , appendKeys
    )
  where

import Control.Arrow
import qualified Data.Map as M

import XMonad

-- Map function over keys's Map .
mapKeys :: (M.Map (ButtonMask, KeySym) (X ()) -> M.Map (ButtonMask, KeySym) (X ()))
           -> XConfig a -> XConfig a
mapKeys f conf      = conf {keys = fmap f (keys conf)}

-- Define each key second time with different modMask (obtained by passing
-- current modMask to function). Suitable for use with mapKeys .
addModMask :: (ButtonMask -> ButtonMask)
                     -> M.Map (ButtonMask, KeySym) (X ())
                     -> M.Map (ButtonMask, KeySym) (X ())
addModMask f xs     = xs `M.union` M.mapKeys (first f) xs

-- Variant of additionalKeys, which appends new key definition to the existing
-- one instead of overwriting it. That means, that single key will executing
-- both old and new actions.
additionalKeys' :: XConfig a -> [((ButtonMask, KeySym), X ())] -> XConfig a
additionalKeys' xcf ys  = xcf { keys = M.unionWith (>>) (M.fromList ys)
                                        . keys xcf
                              }

-- Merge values for the same key (ButtonMask, KeySym) in the list. I may need
-- this, because if list contains several values for the same key, `fromList`
-- will overwrite previous value with the last one, when constructing Map.
appendKeys :: [((ButtonMask, KeySym), X())] -> [((ButtonMask, KeySym), X ())]
appendKeys          = M.toList . foldr (uncurry (M.insertWith (>>))) M.empty

