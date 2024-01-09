import Data.Array (Ix, listArray, (!), range)

import Control.Monad.ST (ST, runST)
import Data.Array.ST (STArray, newArray, readArray, writeArray)

main = let n = 0 in mapM_ (\(str,f) -> putStrLn $ str<>": "<>show (f n))
       [ ("Recursive",fibRecursive)
       , ("Iterative / tailrec.",fibIterative)
       , ("Using iterate",fibIterate)
       , ("Using zipWith",fibZip)
       , ("Using scanl", fibScan)
       , ("Using list as rec. lookup",fibList)
       , ("Using array as rec. lookup",fibArray)
       , ("Using fixpoint operator for rec.",fibFix)
       , ("Using memoizing fixpoint operator for rec.",fibMemo)
       --, ("Using Binet's formula",fibBinet)
       --, ("Using exponentiation and rounding",fibRound)
       , ("Using matrix multiplication",fibVector)
       , ("Using fast matrix exponentiation",fibMatrix)
       , ("Using the ST monad",fibST)
       ]

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
        (a,b, c,d) .@ (e,f, g,h) =
          (a*e + b*g, a*f + b*h,
           c*e + d*g, c*f + d*h)
        power m n
          | n == 0 = (1,0, 0,1)
          | even n = let h = power m (n`div`2) in h .@ h
          | odd  n = m .@ power m (n-1)

{-FIXME for n>10
fibNice = (\(_,x,_,_) -> x) . pow id (0,1, 1,1)
  where (a,b,c,d) @ (e,f,g,h) = (a*e+b*g, a*f+b*h, c*e+d*g, c*f+d*h)
        join = (>>= id)
        pow f m n = if n == 0 then f (1,0, 0,1) else pow ((if odd n then (@) m else id) . join (@) . f) m (n`div`2)-}

fibST n = runST fib where
  fib :: ST s Int
  fib = do
    (vars :: STArray s Char Int) <- newArray ('a','z') 0
    writeArray vars 'a' 0
    writeArray vars 'b' 1
    writeArray vars 'n' n
    fibLoop vars
    a <- readArray vars 'a'
    return a
  fibLoop :: STArray s Char Int -> ST s ()
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

-- Inverse Fibonacci

indexOf 0   = 0
indexOf 1   = error "Fibonacci sequence has two '1's"
indexOf fib = round (logBase phi (sqrt 5 * fib)) -- Precision break at 1475
  where phi = (1 + sqrt 5) / 2
