import Data.Array (Ix, listArray, (!), range)

fix :: ((a -> b) -> (a -> b)) -> (a -> b)
fix f = let y = f y in y

fixMemo :: (Num a, Enum a, Ix a)=> ((a -> b) -> (a -> b)) -> (a -> b)
fixMemo f n = (memoized!) n
  where memoized = listArray (0,n) $ map (f (memoized!)) $ range (0,n)
  --where memoized = array (0,n) [(i,f (memoized!) i) | i<-[0..n]]

-- Fibonnacci numbers

fibRec :: Int -> Integer 
fibRec 0 = 0
fibRec 1 = 1
fibRec n = fibRec (n - 1) +  fibRec (n - 2)

fibIter :: Int -> Integer
fibIter n = fst (fibStep n)
  where fibStep 0 = (0, 1)
        fibStep n = step (fibStep (n-1))
        step (a, b) = (b, a + b)

fibZip :: Int -> Integer -- Favourite
fibZip n = fibs !! n
  where fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

fibScan :: Int -> Integer
fibScan n = fibs !! n
  where fibs = 0 : scanl (+) 1 fibs

fibFix :: Integer -> Integer
fibFix = fix fiblogic
  where fiblogic = \rec n -> if n<=1 then n else rec (n-1) + rec (n-2)

fibDyn :: Int -> Integer
fibDyn n = fibs !! n
  where fibs  = map fib [0..n]
        fib 0 = 0
        fib 1 = 1
        fib n = fibs !! (n-1) + fibs !! (n-2)
  
fibArr :: Int -> Integer
fibArr n = fibs ! n
  where fibs = listArray (0,n) (map fib [0..n])
        fib 0 = 0
        fib 1 = 1
        fib n = fibs ! (n-1) + fibs ! (n-2)

fibMemo :: Integer -> Integer
fibMemo = fixMemo fiblogic
  where fiblogic = \rec n -> if n<=1 then n else rec (n-1) + rec (n-2)


--------------------------------------------------------------------------------


-- Prime numbers

aaronPrime :: Int -> Bool
aaronPrime n=[i|i<-[2..n],n`mod`i<1]==[n]
-- aaronPrime n=[i|i<-[2..n],mod n i<1]==[n]

prime :: Int -> Bool
prime n
  | n <= 1    = False
  | otherwise = null [x | x <- [2..isqrt n], n `mod` x == 0]
-- prime n = n > 1 && and [n `mod` x /= 0 | x <- [2..isqrt n]] -- Oneliner optimised list comprehension
-- prime n = [1,n] == [x | x <- [1..n], n `mod` x == 0] -- Naïve list comprehension

isPrime :: Int -> Bool
isPrime 1 = False
isPrime n = all (\i -> n `mod` i /= 0) [2..n-1]
-- isPrime n -- Simple recursive functions Master Solution
--   | n > 1     = aux 2
--   | otherwise = False
--   where
--     aux m
--       | n `div` 2 < m = True
--       | otherwise     = n `mod` m /= 0 && aux (m + 1)

primes :: [Int]
primes = sieve [2..]
  where sieve (x:xs) = x : sieve [y | y<-xs, y`mod`x /= 0] -- Eratosthenes
--primes = sieve [2..]
--  where sieve (x:xs) = x : sieve (filter ((/=0).(`mod`x)) xs) -- Eratosthenes

isqrt :: Int -> Int
isqrt n = search 0 (n+1)
  where search a b
          | a+1 == b  = a
          | n < p^2   = search a p
          | otherwise = search p b
          where p = (a+b) `div` 2
-- isqrt = floor . sqrt . fromIntegral -- Quick and dirty


--------------------------------------------------------------------------------


-- Collatz

collatzFun = traverse print . monotonous . flip zip [1..] . map collatz $ [1..]
  where
    monotonous :: (Ord a)=> [a] -> [a]
    monotonous (a:b:cs)
      | a >= b    =     monotonous (a:cs)
      | otherwise = a : monotonous (b:cs)
    monotonous as = as
    collatz :: Integer -> Integer
    collatz n = if n==1 then 0 else succ $ collatz (if even n then (n`div`2) else 3*n+1)
    -- collatz :: Integer -> Integer
    -- collatz = collatzStep 0
    -- collatzStep i n = if n==1 then i else collatzStep (i+1) (if even n then (n`div`2) else 3*n+1)
