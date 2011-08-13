import qualified Prelude

-- fmap id = id 
-- fmap (g . h) = fmap g . fmap h
class Functor f where
  fmap :: (a -> b) -> f a -> f b

-- fmap g . pure = pure .g
class Functor f => Pointed f where
  pure :: a -> f a

-- fmap g x = pure g <*> x
class Pointed f => Applicative f where
  (<*>) :: f (a -> b) -> f a -> f b

class Applicative f => Monad f where
  (>>=) :: f a -> (a -> f b) -> f b
