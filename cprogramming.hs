{-# LANGUAGE ImpredicativeTypes #-}
--{-# LANGUAGE AllowAmbiguousTypes #-}

-- Taken from the following article:
-- augustss.blogspot.com/2007/08/programming-in-c-ummm-haskell-heres.html

import Data.IORef (IORef, newIORef, readIORef, writeIORef)

{-main = do TODO fix all this man
  v <- fac 10
  x <- runE v
  print x-}

data E' v a where
  E :: IO a -> E' RValue a
  V :: IO a -> (a -> IO ()) -> E' v a

data LValue
type V a = E' LValue a

data RValue
type E a = E' RValue a

runE :: E' v a -> IO a
runE (E load)   = load
runE (V load _) = load

auto :: E a -> IO (forall v. E' v a)
auto x = do
  x' <- runE x
  r <- newIORef x'
  return (V (readIORef r) (writeIORef r))

one :: E Int
one = E $ return 1

plus :: E Int -> E Int -> E Int
plus x y = E $ do
  x' <- runE x
  y' <- runE y
  return (x' + y')

(=:) :: V a -> E a -> IO ()
(V _ store) =: e = do
  e' <- runE e
  store e'

(*=) :: (Num a)=> V a -> E' v a -> IO ()
(V load store) *= e = do
  e' <- runE e
  v  <- load
  store (v * e')

(-=) :: (Num a)=> V a -> E' v a -> IO ()
(V load store) -= e = do
  e' <- runE e
  v  <- load
  store (v - e')

(>.) :: (Ord a)=> E' v a -> E' v a -> IO Bool
x >. y = (>) <$> runE x <*> runE y

while :: IO Bool -> IO a -> IO ()
while cnd stmt = do
  b <- cnd
  if b then do
    stmt
    while cnd stmt
  else do
    return ()

instance Num a => Num (E a) where
  x + y = E ((+) <$> runE x <*> runE y)
  x * y = E ((*) <$> runE x <*> runE y)
  abs x = E (abs <$> runE x)
  signum x = E (signum <$> runE x)
  fromInteger n = E (return (fromInteger n))
  negate x = E (negate <$> runE x)

--fac :: Int -> IO (E Int)
fac n = do {
  a <- auto 1;
  i <- auto n;
  while (i >. 0) $ do {
    a *= i;
    i -= 1;
  };
  return a;
}

{-NOTE TODO how?
test = do
  x <- auto one;
  x =: x `plus` x;
  x;
  --(x `plus` x) =: one -- Should Error-}

{-NOTE translation, from the paper
fac' n = do
  a <- newIORef 1
  i <- newIORef n
  whileM (do i' <- readIORef i; return $ i' > 0) $ do
    i' <- readIORef i
    a' <- readIORef a
    writeIORef a (a' * i')
    writeIORef i (i' - 1)
  a' <- readIORef a
  return a'
-}

{-NOTE naïve, old implementation
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

while :: ST s Bool -> ST s a -> ST s ()
while bexp stmt = do
  b <- bexp
  if b then do
    stmt
    while bexp stmt
  else do
    return ()

(>.) :: STRef s Int -> Int -> ST s Bool
iRef >. n = do
  i <- readSTRef iRef
  return (i > n)

(*=) :: STRef s Int -> STRef s Int -> ST s ()
aRef *= iRef = do
  a <- readSTRef aRef
  i <- readSTRef iRef
  writeSTRef aRef (a * i)

(-=) :: STRef s Int -> Int -> ST s ()
iRef -= n = do
  i <- readSTRef iRef
  writeSTRef iRef (i - n)

ret :: STRef s Int -> ST s Int
ret aRef = do
  a <- readSTRef aRef
  return a

fac :: Int -> Int
fac n = runST $ do {
    a <- newSTRef 1;
    i <- newSTRef n;
    while (i >. 0) $ do {
        a *= i;
        i -= 1;
    };
    ret a;
  }
-}
