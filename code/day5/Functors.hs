--------------------------------------------------------------------------------
-- Funtors Review
--------------------------------------------------------------------------------

import Control.Applicative

{-
class Functor f => Applicate f where
  pure :: a -> m a
  (<*>) :: m (a -> b) -> m a -> m b
-}

(pure f) <*> fa = fmap f fa

zapp :: [a -> b] -> [a] -> [b]
zapp (f:fs) (x:xs) = f x : zapp fs xs
zapp _ _ = []

{-

instance Applicative Maybe where
   -- pure :: a -> Maybe a
      pure = Just

   -- (<*>) :: Maybe (a -> b) -> Maybe a -> Maybe b
      (Just f) <*> (Just x) = Just (f x)
      _   <*> _ = Nothing

-}
