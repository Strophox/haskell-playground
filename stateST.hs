-- Setup ST monad
import Control.Monad.ST (ST, runST)
import Data.STRef (STRef, newSTRef, readSTRef, writeSTRef)

-- Setup State monad (cf. Control.Monad.State.Lazy)
data State s a = St { runState :: s -> (s,a) }
instance Functor (State s) where
  fmap f st = St $ \s -> let (s',a) = runState st s in (s',f a)
instance Applicative (State s) where
  pure a = St $ \s -> (s,a)
  stf <*> sta = St $ \s -> let {(s', f) = runState stf s ; (s'',a) = runState sta s'} in (s'', f a)
instance Monad (State s) where
  return = pure
  st >>= f = St $ \s -> let (s', a) = runState st s in runState (f a) s'
load :: State s s
load = St $ \s -> (s,s)
store :: s -> State s ()
store s = St $ \_ -> (s,())

main = do
  putStrLn "Renaming manually:"
  print (renameManual exTree)
  putStrLn "Renaming with State:"
  print (renameState exTree)
  putStrLn "Renaming with ST:"
  print (renameST exTree)

data Tree a = Leaf a | Tree a :|: Tree a
  deriving (Eq,Show)

exTree :: Tree Char
exTree = (Leaf 'a' :|: (Leaf 'b' :|: Leaf 'c')) :|: (Leaf 'd' :|: Leaf 'e')

-- Manually thread state (i :: Int) to rename tree
renameManual :: Tree a -> Tree Int
renameManual tree = snd $ proc tree 0
  where
    proc :: Tree a -> Int -> (Int, Tree Int)
    proc (Leaf a) i = (i+1, Leaf i)
    proc (left :|: right) i = let
        (i', newLeft)  = proc left i
        (i'',newRight) = proc right i'
        in (i'', newLeft :|: newRight)

-- Use monad to thread state (i :: Int) to rename tree
renameState :: Tree a -> Tree Int
renameState tree = snd $ runState (proc tree) 0
  where
    proc :: Tree a -> State Int (Tree Int)
    proc (Leaf a) = do
        i <- load
        store (i+1)
        return (Leaf i)
    proc (left :|: right) = do
        newLeft  <- proc left
        newRight <- proc right
        return (newLeft :|: newRight)

-- Use ST monad to thread state references (iRef :: STRef s Int) to rename tree
renameST :: Tree a -> Tree Int
renameST tree = runST (proc tree =<< newSTRef 0)
  where
    proc :: Tree a -> STRef s Int -> ST s (Tree Int)
    proc (Leaf a) iRef = do
        i <- readSTRef iRef
        writeSTRef iRef (i+1)
        return (Leaf i)
    proc (left :|: right) iRef = do
        newLeft  <- proc left iRef
        newRight <- proc right iRef
        return (newLeft :|: newRight)
