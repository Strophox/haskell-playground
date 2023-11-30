-- Monads II   https://www.youtube.com/watch?v=YDj20ySKWP8

{-------------------------------------------------------------------------------

Consider
  Char -> Int

       +------+
    c  |      |  i
  ---->|  st  |---->
       |      |
       +------+

A state transformer is...
  type ST a = State -> (a, State)

       +------+  v
       |      |---->
       |  st  |
  ---->|      |---->
    s  +------+  s'

Consider
  Char -> ST Int = Char -> State -> (Int,State)

    c  +------+  i
  ---->|      |---->
       |  st  |
  ---->|      |---->
    s  +------+  s'

-------------------------------------------------------------------------------}

type State = Int -- Arbitrary

newtype ST a = S { app :: State -> (a,State) }
-- app :: ST a -> (State -> (a,State))
-- app (S st) s = st s

instance Functor ST where
  -- fmap :: (a -> b) -> ST a -> ST b
  fmap f sta = S $ \s -> let (a,s') = app sta s in (f a, s')

instance Applicative ST where
  -- pure :: a -> ST a
  pure x = S $ \s -> (x,s)
  -- (<*>) :: ST (a -> b) -> ST a -> ST b
  (<*>) stf sta = S $ \s -> let (f,s') = app stf s in
                            let (a,s'') = app sta s' in
                            (f a, s'')


instance Monad ST where
  -- return :: a -> ST a
  return = pure
{-------------------------------------------------------------------------------

    a  +------+  a
  ---->|      |---->
       |  st  |
  ---->|      |---->
    s  +------+  s

-------------------------------------------------------------------------------}
  -- (>>=) = ST a -> (a -> ST b) -> ST b
  (>>=) st f = S $ \s -> let (a,s') = app st s in
                         app (f a) s'
{-------------------------------------------------------------------------------

       +------+  a  +------+  b
       |      |---->|      |---->
       |  st  |     |   f  |
  ---->|      |---->|      |---->
    s  +------+  s' +------+  s''

-------------------------------------------------------------------------------}


-- Monads III  https://www.youtube.com/watch?v=WYysg5Nf7AU
data Tree a = Leaf a | Node (Tree a) (Tree a)

t :: Tree Char
t = Node (Node (Leaf 'a') (Leaf 'b')) (Leaf 'c')

-- Relabel a tree with manual plumbing
rlabel :: Tree a -> Int -> (Tree Int, Int)
rlabel (Leaf x) n   = (Leaf n, n+1)
rlabel (Node l r) n = (Node l' r', n'')
  where (l',n')  = rlabel l n
        (r',n'') = rlabel r n'

-- type State = Int

fresh :: ST Int
fresh = S $ \s -> (s, s+1)

-- Label a tree with the state monad
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf x)   = fresh >>= \n -> return (Leaf n)
mlabel (Node l r) = mlabel l >>= \l' -> mlabel r >>= \r' -> return (Node l' r')
{-
mlabel :: Tree a -> ST (Tree Int)
mlabel (Leaf x)   = do n <- fresh
                       return (Leaf n)
mlabel (Node l r) = do l' <- mlabel l
                       r' <- mlabel r
                       return (Node l' r')
-}

-- Toplevel relabeling function. No monads visible to user here.
label :: Tree a -> Tree Int
label t = fst (app (mlabel t) 0)

{-------------------------------------------------------------------------------

Effectful programming.
  a -> Maybe b    Exceptions
  a -> ([]) b     Non-determinism
  a -> ST b       Internal state
  a -> IO b       Input/Output

-------------------------------------------------------------------------------}

{-
import Control.Arrow

type State = Int -- Arbitrary
newtype ST a = S { app :: State -> (a,State) }

instance Functor ST where
--fmap :: (a -> b) -> ST a -> ST b
  fmap f sta = S (first f . app sta)
  -- fmap f sta = S $ \s -> let (a,s') = app sta s in (f a, s')

instance Applicative ST where
--pure :: a -> ST a
  pure x = S (\s -> (x,s))
--(<*>) :: ST (a -> b) -> ST a -> ST b
  (<*>) = S ((\(f,(a,s'') -> (f a, s'')) . second (app sta) . app stf)
  -- (<*>) stf sta = S $ \s -> let (f,s') = app stf s in let (a,s'') = app sta s' in (f a, s'')

instance Monad ST where
  return = pure
  (>>=) :: ST a -> (a -> ST b) -> ST b
  (>>=) sta f = S (uncurry app . first f . app sta)
  -- (>>=) sta f = S $ \s -> let (a,s') = app sta s in app (f a) s'
-}
