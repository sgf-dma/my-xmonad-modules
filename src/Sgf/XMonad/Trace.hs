
module Sgf.XMonad.Trace
    ( traceWindowSet
    , traceFloat
    )
  where

import Data.Maybe
import qualified Data.Map as M
import Control.Applicative
import Control.Monad

import XMonad
import qualified XMonad.StackSet as W


-- Log all windows (tiled and floating).
traceWindowSet :: X ()
traceWindowSet      = do
    trace "Windows:"
    withWindowSet $ \ws -> do
      whenJust (W.stack . W.workspace . W.current $ ws) $ \s -> do
        ts <- mapM (runQuery title) (W.integrate s)
        trace $ "Tiled: " ++ show ts
      fs <- mapM (runQuery title) (M.keys . W.floating $ ws)
      unless (null fs) $ trace ("Floating: " ++ show fs)

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

