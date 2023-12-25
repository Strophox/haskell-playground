import Data.Array (Ix, listArray, (!), range)

import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, newArray, readArray, writeArray)

indexOf 0   = 0
indexOf fib = round (logBase phi (sqrt 5 * fib))
  where phi = (1 + sqrt 5) / 2

-- Fibonacci implementations

fibRecursive 0 = 0
fibRecursive 1 = 1
fibRecursive n = fibRecursive (n - 1) +  fibRecursive (n - 2)

fibIterative n = fst (fib n)
  where fib n
          | n == 0    = (0, 1)
          | otherwise = fibStep (fib (n-1))
        fibStep (a, b) = (b, a + b)

fibIterate n = fst (iterate fibStep (0, 1) !! n)
  where fibStep (a, b) = (b, a + b)

fibZip n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibScan n = fibs !! n
  where fibs = 0 : scanl (+) 1 fibs

fibList n = fibs !! n
  where fibs  = map fib [0..n]
        fib 0 = 0
        fib 1 = 1
        fib n = fibs !! (n-1) + fibs !! (n-2)

fibArray n = fibs ! n
  where fibs = listArray (0,n) (fib <$> range (0,n))
        fib 0 = 0
        fib 1 = 1
        fib n = fibs ! (n-1) + fibs ! (n-2)

fibFix n = (fix fiblogic) n
  where fiblogic = \rec n -> if n<=1 then n else rec (n-1) + rec (n-2)
        fix :: ((a -> b) -> (a -> b)) -> (a -> b)
        fix f x = f (fix f) x

fibMemo n = fixMemo fiblogic n
  where fiblogic = \rec n -> if n<=1 then n else rec (n-1) + rec (n-2)
        fixMemo :: (Num a, Enum a, Ix a)=> ((a -> b) -> (a -> b)) -> (a -> b)
        fixMemo f x = f (table!) x
          where table = listArray (0,x) (f (table!) <$> range (0,x))

fibBinet n = (phi**n - psi**n) / (phi - psi)
  where phi = (1 + sqrt 5) / 2
        psi = (1 - sqrt 5) / 2

fibRound n = round (phi**n / sqrt 5)
  where phi = (1 + sqrt 5) / 2

fibVector n = fst (iterate ((0,1, 1,1).*) (0, 1) !! n)
  where (a,b, c,d) .* (e, f) = (a*e + b*f, c*e + d*f)

fibMatrix n = fst $ power (0,1, 1,1) n .* (0, 1)
  where (a,b, c,d) .* (e, f) = (a*e + b*f, c*e + d*f)
        (a,b, c,d) .# (e,f, g,h) =
          (a*e + b*g, a*f + b*h,
           c*e + d*g, c*f + d*h)
        power m n
          | n == 1 = m
          | even n = let h = power m (n`div`2) in h .# h
          | odd  n = m .# power m (n-1)

fibFast n = -- TODO simplify Matrix method!

fibST n = runST fib where
  fib :: ST s Integer
  fib = do
    (vars :: STArray s Char Integer) <- newArray ('a','z') 0
    writeArray vars 'a' 0
    writeArray vars 'b' 1
    writeArray vars 'n' n
    fibLoop vars
    a <- readArray vars 'a'
    return a
  fibLoop :: STArray s Char Integer -> ST s ()
  fibLoop vars = do
    n <- readArray vars 'n'
    if n == 0 then do
      return ()
    else do
      a <- readArray vars 'a'
      b <- readArray vars 'b'
      n <- readArray vars 'n'
      writeArray vars 'a' b
      writeArray vars 'b' (a + b)
      writeArray vars 'n' (n - 1)
      fibLoop vars
