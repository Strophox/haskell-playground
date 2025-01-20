import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, newArray, readArray, writeArray)

fibImp n = runST fib where
  fib :: ST s Integer
  fib = do
    (vars :: STArray s Char Integer) <- newArray ('a','z') 0
    writeArray vars 'a' 0
    writeArray vars 'b' 1
    writeArray vars 'n' n
    while ((0<) <$> readArray vars 'n') $ do
        a <- readArray vars 'a'
        b <- readArray vars 'b'
        n <- readArray vars 'n'
        writeArray vars 'a' b
        writeArray vars 'b' (a + b)
        writeArray vars 'n' (n - 1)
    a <- readArray vars 'a'
    return a
  while :: ST s Bool -> ST s a -> ST s ()
  while cnd stmt = do
    b <- cnd
    if not b then
      return ()
    else do
      stmt
      while cnd stmt
