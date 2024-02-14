-- Inspired by https://stackoverflow.com/questions/13352205/what-are-free-monads

data Free f a = Pure a | Roll (f (Free f a))

instance Functor f => Functor (Free f) where
-- fmap :: (a -> b) -> Free f a -> Free f b
  fmap f (Pure x) = Pure (f x)
  fmap f (Roll v) = Roll (fmap (fmap f) v)

instance Functor f => Applicative (Free f) where
-- pure :: a -> Free f a
  pure = Pure
-- (<*>) :: Free f (a -> b) -> Free f a -> Free f b
  (Roll v) <*> u = Roll (fmap (<*> u) v)
  v <*> (Roll u) = Roll (fmap (v <*>) u)
  (Pure f) <*> (Pure x) = Pure (f x)

instance Functor f => Monad (Free f) where
  return = pure
-- (>>=) :: Free f a -> (a -> Free f b) -> Free f b
  (Pure x) >>= f = f x
  (Roll v) >>= f = Roll (fmap (>>= f) v)
  {-m >>= f = join (fmap f m)
    where join :: Free f (Free f a) -> Free f a
          join (Pure x) = x
          join (Roll v) = Roll (fmap join v)-}

liftFree :: Functor f => f a -> Free f a
liftFree v = Roll (fmap Pure v)

foldFree :: Functor f => (f r -> r) -> Free f r -> r
foldFree g (Pure x) = x
foldFree g (Roll v) = g (fmap (foldFree g) v)
