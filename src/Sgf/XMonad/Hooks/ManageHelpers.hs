
module Sgf.XMonad.Hooks.ManageHelpers
    ( composeOne
    , (-?>)
    )
  where


composeOne :: (Monoid a, Monad m) => [m (Maybe a)] -> m a
composeOne [] = return mempty
composeOne (mx : xs) = do
    x <- mx
    case x of
      Just y  -> return y
      Nothing -> composeOne xs

infixr 0 -?>
(-?>) :: Monad m => m Bool -> m a -> m (Maybe a)
(-?>) mb mx     = do
    b <- mb
    if b
      then Just <$> mx
      else return Nothing

