-- Prime numbers

aaronPrime :: Int -> Bool
aaronPrime n=[i |i<-[2..n],n`mod`i<1]==[n] -- no space before | breaks highlighting??!
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


--------------------------------------------------------------------------------

buildInt :: (Foldable t,Num a)=> t a -> a
buildInt = foldl1 (\b a -> 10*b + a)


---

{-
-- Introduce continuation as accumulating parameter:
--fac' n k = k (fac n)
-- Then calculate:
fac :: Integer -> Integer
fac n = fac' n id
  where
    fac' :: Integer -> (Integer -> Integer) -> Integer
    fac' 0 k = k 1
    fac' n k = fac (n-1) (\m -> k (n * m))
-- Now tail-recursive, but higher-order.
-}
