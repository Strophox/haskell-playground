import Control.Monad.ST (ST,runST)
import Data.Array.ST (STArray,newArray,readArray,writeArray)

main = do
  let (a,b) = runST proc
  print (a*b)

proc :: ST s (Int,Int)
proc = do
  (nums :: STArray s Char Int) <- newArray ('a','b') 0
  writeArray nums 'a' 6
  a <- readArray nums 'a'
  subproc nums
  b <- readArray nums 'b'
  return (a,b)

subproc :: STArray s Char Int -> ST s ()
subproc nums = do
  writeArray nums 'b' 7
  writeArray nums 'a' 251
