-- Practice implementations for some functions:
-- PRELUDE -- https://hackage.haskell.org/package/base-4.11.1.0/docs/Prelude.html

-- Booleans
(&&?) :: Bool -> Bool -> Bool
(&&?) True True = True
(&&?) _    _    = False
(||?) :: Bool -> Bool -> Bool
(||?) False False = False
(||?) _     _     = True
not' :: Bool -> Bool
not' b = maybe undefined id (lookup b [(True,False),(False,True)])
bool' :: a -> a -> Bool -> a
bool' x y b = if b then x else y

-- Comparisons
-- (==), (/=) :: (Eq a)=> a -> a -> Bool
-- (<),(<=),(>),(>=) :: (Ord a)=> a -> a -> Bool
-- compare :: (Ord a)=> a -> a -> Ordering
-- min, max :: (Ord a)=> a -> a -> a

-- Integers
-- (+), (-), (*) :: (Num a)=> a -> a -> a
-- (^) :: (Integral b, Num a)=> a -> b -> a
even' :: Int -> Bool -- even :: (Integral a)=> a -> Bool
even' n = n `mod` 2 == 0
-- even' = (==0).(`mod`2)
odd' :: Int -> Bool --  odd :: (Integral a)=> a -> Bool
odd' = not . even
negate' :: Int -> Int -- negate :: (Num a)=> a -> a
negate' x = -x
abs' :: Int -> Int -- abs :: (Num a)=> a -> a
abs' x = if x < 0 then negate x else x
signum' :: Int -> Int -- signum :: (Num a)=> a -> a
signum' x = if x == 0 then 0 else x`div`(abs x)
subtract' :: Int -> Int -> Int -- subtract :: (Num a)=> a -> a -> a
subtract' = flip (-)
div' :: Int -> Int -> Int -- div :: (Integral a)=> a -> a -> a
div' = undefined -- Integer division truncated to -infty
mod' :: Int -> Int -> Int -- mod :: (Integral a)=> a -> a -> a
mod' = undefined-- (x `div` y)*y + (x `mod` y) == x
quot' :: Int -> Int -> Int -- quot :: (Integral a)=> a -> a -> a
quot' = undefined -- Integer division truncated to zero
rem' :: Int -> Int -> Int -- rem :: (Integral a)=> a -> a -> a
rem' = undefined -- (x `quot` y)*y + (x `rem` y) == x
--divMod, quotRem :: (Integral a)=> a -> a -> (a,a)
gcd' :: Int -> Int -> Int -- lcm :: (Integral a)=> a -> a -> a
gcd' n m -- Greatest common divisor
  | n == m = n
  | n > m  = gcd (n-m) m
  | n < m  = gcd n (m-n)
lcm' :: Int -> Int -> Int -- lcm :: (Integral a)=> a -> a -> a
lcm' n m = n * m `div` gcd n m -- (gcd x y) * (lcm x y) == x * y

-- Floats
--pi :: (Floating a)=> a
--(/) :: (Floating a)=> a -> a -> a
--(**) :: (Floating a)=> a -> a -> a
--(^^) :: (Fractional a,Integral b)=> a -> b -> a
recip' :: Double -> Double -- recip :: (Fractional a)=> a -> a
recip' = (1/)
--sqrt, exp, log :: (Floating a)=> a -> a
--logBase :: (Floating a)=> a -> a -> a
--sin, cos, tan :: (Floating a)=> a -> a
--asin, acos, atan :: (Floating a)=> a -> a
--atan2 :: (Floating a)=> a -> a -> a
--sinh, cosh, tanh, asinh, acosh, atanh :: (Floating a)=> a -> a
--properFraction :: (RealFrac a, Integral b)=> a -> (b, a)

-- Number Conversions
--floor, ceiling, round, truncate :: (RealFrac a, Integral b)=> a -> b
--fromIntegral :: (Integral a, Num b)=> a -> b
--fromInteger :: (Num b)=> Integer -> b
--toInteger :: (Integral a)=> a -> Integer
--toRational :: (Real a)=> a -> Rational
--fromRational :: (Fractional a)=> Rational -> a
--realToFrac :: (Real a, Fractional b)=> a -> b

-- Strings
words' :: String -> [String]
words' "" = []
words' cs = ($cs) $ filter (/="") . uncurry (:) . fmap words' . break (==' ') . dropWhile (==' ')
-- words' str = let s = dropWhile (==' ') str in if null s then [] else (uncurry (:) . fmap words' . break (==' ')) s
words'f :: String -> [String]
words'f = foldr (\x->if x/="" then (x:) else id) [] . foldr (\c rec -> let (r:rs)=rec in if c==' ' then "":r:rs else (c:r):rs) [""]
words'p :: String -> [String]
words'p s  = case dropWhile (==' ') s of -- From Prelude
  "" -> []
  s' -> w : words'p s'' where (w,s'') = break (==' ') s'
testwords :: IO ()
testwords = mapM_ tester testset
  where
    testset = ["basic","test one two",""," ","   ","prefix  ","  infix  ","  suffix"," C. C. ","palindrome = reverse >>= (==)"]
    tester = (\input -> let
        (expect : result : _) = [words, words'] <*> [input]
        passed = expect==result
      in
        putStr
        . showString"• " . showString(if passed then "passed" else "FAILED") . showString" " . shows input . showString"\n"
        . showString"  " . showString"got: " . shows result . showString"\n"
        . (if passed then id else
          showString"  " . showString"expected: " . shows expect . showString"\n")
        $ [])
unwords' :: [String] -> String
unwords' [] = ""
unwords' xs = foldr1 (\x acc -> x++" "++acc) xs
-- unwords' [] = ""
-- unwords' (x:[]) = x
-- unwords' (x:xs) = x ++ " " ++ unwords' xs
-- unwords' = drop 1 . concatMap (' ':)
--lines :: String -> [String]
--unlines :: [String] -> String
--read :: (Read a)=> String -> a
--show :: (Show a)=> a -> String

-- Lists I (`basic parts')
infixr 5 :? -- (:) :: a -> [a] -> [a]
data List a = Nil | a :? List a
  deriving (Eq, Ord, Read, Show) -- (Eq, Read, Show, Ord, Enum, Bounded)
-- data List a where
--   Nil :: List a
--   (:?) :: a -> List a -> List a
--   deriving (Eq, Ord)
infixr 5 ++?
(++?) :: [a] -> [a] -> [a]
(++?) = flip (foldr (:))
-- []     ++? ys = ys
-- (x:xs) ++? ys = x : (xs ++? ys)
infixl 9 !!?
(!!?) :: [a] -> Int -> a
(!!?) (x:xs) 0 = x
(!!?) (x:xs) n = xs !!? (n-1)
-- (!!?) :: [a] -> Int -> a
-- (!!?) list i = head (drop i list)
head' :: [a] -> a
head' = foldr const (error "empty list")
-- head' (x:_) = x
-- head' [] = error "empty list"
tail' :: [a] -> [a]
tail' [] = error "empty list"
tail' (_:xs) = xs
init' :: [a] -> [a]
init' [] = error "empty list"
init' (x:[]) = []
init' (x:xs) = x : init' xs
last' :: [a] -> a
last' = foldr1 (flip const)
-- last' (x:[]) = x
-- last' (x:xs) = last' xs
-- last' [] = error "empty list"
take' :: Int -> [a] -> [a]
take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : (take' (n-1) xs)
drop' :: Int -> [a] -> [a]
drop' n l = foldr (\x acc -> if length acc < length l - n then x:acc else acc) [] l
-- drop' 0 xs = xs
-- drop' _ [] = []
-- drop' n (x:xs) = drop' (n-1) xs
splitAt' :: Int -> [a] -> ([a], [a])
splitAt' n xs = (take n xs, drop n xs)
-- splitAt' _ [] = ([], [])
-- splitAt' 0 xs = ([], xs)
-- splitAt' n (x:xs) = (\(first, rest) -> (x:first, rest)) (splitAt' (n-1) xs)
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) = if p x then x:(takeWhile' p xs) else []
-- takeWhile' p [] = []
-- takeWhile' p (x:xs)
--   | p x       = x : (takeWhile' p xs)
--   | otherwise = []
dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) = if p x then dropWhile' p xs else (x:xs)
-- dropWhile' p [] = []
-- dropWhile' p (x:xs)
--   | p x       = dropWhile' p xs
--   | otherwise = (x:xs)
span' :: (a -> Bool) -> [a] -> ([a], [a])
span' p xs = (takeWhile p xs, dropWhile p xs)
-- span' p [] = ([], [])
-- span' p (x:xs) = if not (p x) then ([], x:xs) else let (a, b) = span p xs in (x:a, b)
break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p xs = span (not . p) xs

-- Lists II (`modify')
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []
-- map' f xs = [ f x | x <- xs ]
-- map' f = (>>= return . f)
-- map' = fmap
-- map' f [] = []
-- map' f (x:xs) = f x : (map' f xs)
filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x -> if p x then (x:) else id) []
-- filter' p xs = [ x | x <- xs, p x ]
-- filter' p [] = []
-- filter' p (x:xs)
--   | p x       = x : filter' p xs
--   | otherwise = filter' p xs
-- filter' p (x:xs) = if p x then x : filter p xs else filter p xs
-- filter' _ _ = []
concat' :: Foldable t => t [a] -> [a]
concat' = foldr (++) []
-- concat' [ ] = []
-- concat' (x:xs) = x ++ (concat xs)
concatMap' :: (a -> [b]) -> [a] -> [b] -- concatMap :: (Foldable l)=> (a -> [b]) -> l a -> [b]
concatMap' f = foldr ((++) . f) []
-- concatMap' f xs = [ c | x <- xs, c <- f x ]
-- concatMap' f = concat . map f
reverse' :: [a] -> [a]
reverse' = foldr (\x -> (++[x])) []
-- reverse' = revers [] -- Tailrecursive
--   where revers acc [] = acc
--         revers acc (x:xs) = revers (x:acc) xs
-- reverse' l = [l!!(length l - i) | i <- [0..length l - 1]] -- Indices, ew
-- reverse' = foldl (flip (:)) []
-- reverse' [] = []
-- reverse' (x:xs) = xs ++ [x]
replicate' :: Int -> a -> [a]
replicate' n x = take n (repeat x)
-- replicate' 0 x = []
-- replicate' n x = x : replicate' (n-1) x
repeat' :: a -> [a]
repeat' x = x : (repeat x)
cycle' :: [a] -> [a]
cycle' = concat . repeat
-- cycle' xs = xs ++ cycle' xs

-- Lists III (`reduce')
length' :: [a] -> Int -- length :: (Foldable l)=> l a -> Int
length' = foldr (const (+1)) 0
-- length' = foldr (\_ y -> y+1) 0
-- length' [] = 0
-- length' (_:xs) = 1 + length' xs
null' :: [a] -> Bool -- null :: (Foldable l)=> l a -> Bool
null' = foldr (\_ _->False) True
-- null' [] = True
-- null' _ = False
elem' :: Eq a => a -> [a] -> Bool -- elem :: (Foldable l, Eq a)=> a -> l a -> Bool
elem' x = foldr ((||).(==x)) False
-- elem' x l = or [x==y | y <- l]
-- elem' x = any (==x)
-- elem' x [] = False
-- elem' x (y:ys) = (x == y) || elem' x ys
notElem' :: Eq a => a -> [a] -> Bool -- notElem :: (Foldable l, Eq a)=> a -> l a -> Bool
notElem' x = foldr ((&&).(/=x)) True
-- notElem' x l = and [x/=y | y <- l]
-- notElem' x = all (/=x)
-- notElem' = (not .) . elem
maximum' :: (Ord a)=> [a] -> a -- maximum :: (Foldable l, Ord a)=> l a -> a
maximum' = foldr1 max
-- maximum' (x:xs) = foldl max x xs
-- maximum' [] = error "empty list"
minimum' :: (Ord a)=> [a] -> a -- minimum :: (Foldable l, Ord a)=> l a -> a
minimum' = foldr1 min
-- minimum' (x:xs) = foldl min x xs
-- minimum' [] = error "empty list"
and' :: Foldable t => t Bool -> Bool
and' = foldr (&&) True
-- and' [] = True
-- and' (x:xs) = x && (and' xs)
or' :: Foldable t => t Bool -> Bool
or' = foldr (||) False
-- or' [] = False
-- or' (x:xs) = x || (or' xs)
xor' :: Foldable t => t Bool -> Bool
xor' = foldr (/=) False
all' :: (Foldable l)=> (a -> Bool) -> l a -> Bool
all' p = foldr ((&&).p) True
-- all' p = and . map p
-- all' _ [] = True
-- all' p (x:xs) = p x && (all' p xs)
any' :: (Foldable l)=> (a -> Bool) -> l a -> Bool
any' p = foldr ((||).p) False
-- any' p = or . map p
-- any' _ [] = False
-- any' p (x:xs) = p x || (any' p xs)
sum' :: Num a => [a] -> a -- sum :: (Foldable l, Num a)=> l a -> a
sum' = foldr (+) 0
product' :: Num a => [a] -> a
product' = foldr (*) 1 -- product :: (Foldable l, Num a)=> l a -> a
foldr' :: (->) ((->) a ((->) b b)) ((->) b ((->) [a] b)) -- foldr :: (Foldable l)=> (a -> b -> b) -> b -> l a -> b
foldr' = ((head .) .) . scanr -- point-less
-- foldr' :: (a -> b -> b) -> b -> [a] -> b
-- foldr' op a []     = a
-- foldr' op a (x:xs) = x `op` (foldr' op a xs)

-- foldr' combine start list =
--   if null list then
--     start
--   else
--     first `combine` (foldr' combine start rest)
--   where first : rest = list

-- foldr' op a = folder
--   where folder []     = a
--         folder (x:xs) = x `op` folder xs
foldl'' :: (b -> a -> b) -> b -> [a] -> b -- foldl :: (Foldable l)=> (b -> a -> b) -> b -> l a -> b
foldl'' op a xs = foldr (\x' fun a -> fun (op a x')) (\a -> a) xs a

-- foldl f a bs = foldr (\b g x -> g (f x b)) id bs a

-- foldl' f v xs = foldr (\x g -> (\a -> g (f a x))) id xs v

-- foldl' op a []     = a
-- foldl' op a (x:xs) = foldl' op (a`op`x) xs

-- foldl' op = fdlol
--   where fdlol a []     = a
--         fdlol a (x:xs) = fdlol (a `op` x) xs
--foldr1, foldl1 :: (Foldable l)=> (a -> a -> a) -> l a -> a
scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' op a l = foldr (\x (b:bs) -> x`op`b:b:bs) [a] l

-- scanr' op a l = foldr (\x acc@(b:_) -> (x`op`b:acc)) [a] l

-- scanr f q0 []     =  [q0] -- Prelude
-- scanr f q0 (x:xs) =  f x q : qs
--                     where qs@(q:_) = scanr f q0 xs
testscanr scanr = scanr (\x b -> "("++show x++"+"++b++")") "0" [1..4]
scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f z xs = foldr (\x rec z -> z:rec (f z x)) (\z -> [z]) xs z
-- scanl' op a l = reverse $ foldl (\(b:bs) x -> b`op`x:b:bs) [a] l

-- scanl' op a l = reverse $ foldl (\acc@(b:_) x -> (b`op`x:acc)) [a] l

-- scanl' op acc [] = [acc]
-- scanl' op acc (x:xs) = acc : (scanl' op (acc`op`x) xs)

-- scanl' f x xs = reverse (foldl g [x] xs) -- Master solution using fold
--   where g (a:as) x = (f a x):a:as

-- scanl f q xs     =  q : (case xs of -- From Prelude
--                             []   -> []
--                             x:xs -> scanl f (f q x) xs)
testscanl scanl = scanl (\b x -> "("++b++"+"++show x++")") "0" [1..4]
--scanr1, scanl1 :: (a -> a -> a) -> [a] -> [a]

-- Lists IV (`multiple')
zip' :: [a] -> [b] -> [(a,b)]
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
zip' _ _ = []
-- zip' = zipWith (,)
unzip' :: [(a,b)] -> ([a],[b])
unzip' = foldr (\(a, b) (as, bs) -> (a:as, b:bs)) ([], [])
-- unzip' [] = ([], [])
-- unzip' (x:xs) = ((a:as), (b:bs))
--   where (a,b) = x
--         (as,bs) = unzip' xs
-- unzip' [] = ([], [])
-- unzip' (x:xs) = ((fst x : fst (unzip' xs)), (snd x : snd (unzip' xs)))
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f (x:xs) (y:ys) = f x y : (zipWith' f xs ys)
zipWith' _ _ _ = []
-- zipWith' f as bs = map (uncurry f) (zip as bs)
zip3' :: [a] -> [b] -> [c] -> [(a,b,c)]
zip3' (x:xs) (y:ys) (z:zs) = (x,y,z) : (zip3' xs ys zs)
zip3' _ _ _ = []
-- zip3' = zipWith3 (,,)
unzip3' :: [(a,b,c)] -> ([a],[b],[c])
unzip3' = foldr (\(a, b, c) (as, bs, cs) -> (a:as, b:bs, c:cs)) ([], [], [])
-- unzip3' [] = ([], [], [])
-- unzip3' (x:xs) = ((a:as), (b:bs), (c:cs))
--   where (a, b, c) = x
--         (as, bs, cs) = unzip3' xs
zipWith3' :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3' f (x:xs) (y:ys) (z:zs) = (f x y z) : (zipWith3 f xs ys zs)
lookup' :: (Eq a)=> a -> [(a,b)] -> Maybe b
lookup' _ [] = Nothing
lookup' k' ((k,v):kvs) = if k' == k then Just v else lookup' k' kvs

-- Tuples
fst' :: (a,b) -> a
fst' (x, _) = x
snd' :: (a,b) -> b
snd' (_, y) = y
swap' :: (a, b) -> (b, a)
swap' = uncurry (flip (,))
-- swap' (x, y) = (y, x)
--(,) :: a -> b -> (a,b)
--(,,) :: a -> b -> c -> (a,b,c)
--(,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) :: t1 -> ... -> t62 -> (t1, .., t62)

-- General Functions
infixr 9 .?
(.?) :: (b -> c) -> (a -> b) -> (a -> c)
(f .? g) x = f (g x) -- \x -> f (g x)
infixr 0 $?
($?) :: (a -> b) -> a -> b
($?) f x = f x
infixr 0 $!?
($!?) :: (a -> b) -> a -> b
($!?) f x = x `seq` f x
id' :: a -> a
id' x = x
const' :: a -> b -> a
const' x _ = x
flip' :: (a -> b -> c) ->(b -> a -> c)
flip' f = let g x y = f y x in g
-- flip' f y x = f x y
curry' :: ((a,b) -> c) -> (a -> b -> c)
curry' f = let g x y = f (x, y) in g
-- curry' f = g where g x y = f (x, y)
uncurry' :: (a -> b -> c) -> ((a,b) -> c)
uncurry' f = let g (x,y) = f x y in g
-- uncurry' g = f where f (x, y) = g x y
until' :: (a -> Bool) -> (a -> a) -> a -> a
until' p f = head . filter p . iterate f
-- until' p f x = if p x then x else until' p f (f x)
iterate' :: (a -> a) -> a -> [a]
iterate' f x = x : iterate' f (f x)
--asTypeOf :: a -> a -> a
--error, errorWithoutStackTrace :: String -> a
undefined' :: a
undefined' = undefined'
---seq :: a -> b -> b
fix :: (a -> a) -> a
fix f = let x = f x in x
-- fix f = f (fix f) -- Less efficient: compare fix (1:) !! (10^8)
-- fix f = foldr (\_->f) undefined (repeat undefined) -- WTF
testfix = (\f n -> if n==0 then 1 else n * f (n-1))

-- Functors
--fmap :: (Functor f)=> (a -> b) -> f a -> f b
infixl 4 <$>?
(<$>?) :: (Functor f)=> (a -> b) -> f a -> f b
(<$>?) = fmap
infixl 4 <$?
(<$?) :: (Functor f)=> b -> f a -> f b
(<$?) = fmap . const
-- (<$?) x fy = fy >> return x

-- Applicatives
--pure :: (Applicative f)=> a -> f a
-- infixl 4 <*>?
-- (<*>?) :: (Applicative f)=> f (a -> b) -> f a -> f b
-- (<*>?) af ax = do f <- af; x <- ax; pure (f x)
infixl 4 <*?
(<*?) :: (Applicative f)=> f a -> f b -> f a
(<*?) ax ay = const <$> ax <*> ay -- liftA2 const ax ay
infixl 4 *>?
(*>?) :: (Applicative f)=> f a -> f b -> f b
(*>?) ax ay = id <$ ax <*> ay

-- Monads
return' :: (Monad m)=> a -> m a
return' = pure
(>>=?) :: (Monad m)=> m a -> (a -> m b) -> m b -- 'bind', 'andThen', 'flatMap', 'fmapAndJoin'
(>>=?) mx f = do x <- mx ; f x
infixl 1 >>?
(>>?) :: (Monad m)=> m a -> m b -> m b
(>>?) mx my = mx >>= \_ -> my
-- (>>?) mx my = do mx ; my
-- (>>?) = (*>)
infixr 1 =<<?
(=<<?) :: (Monad m)=> (a -> m b) -> m a -> m b
(=<<?) = flip (>>=)
--fail :: (Monad m)=> String -> m a

-- Maybe & Either
maybe' :: b -> (a -> b) -> Maybe a -> b
maybe' b f x = case x of
  Just a -> f a
  Nothing -> b
either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left  a) = f a
either' _ g (Right b) = g b

-- IO
--getChar :: IO Char
--getLine :: IO String
--getContents :: IO String
--putChar :: Char -> IO ()
--putStr, putStrLn :: String -> IO ()
--print :: (Show a)=> a -> IO ()
--interact :: (String -> String) -> IO ()
--userError :: String -> IOError
--ioError :: IOError -> IO a
--readIO :: (Read a)=> String -> IO a
--readLn :: (Read a)=> IO a

-- Files
--writeFile, appendFile :: FilePath -> String -> IO ()
--readFile :: FilePath -> IO String

-- Parsing & Showing
--lex :: ReadS String
--readParen :: Bool -> ReadS a -> ReadS a
--readList :: (Read a)=> ReadS [a]
--readsPrec :: (Read a)=> Int -> ReadS a
--reads :: (Read a)=> ReadS a
type ShowS' = String -> String
showChar' :: Char -> ShowS'
showChar' = (:)
showString' :: String -> ShowS'
showString' = (++)
showParen' :: Bool -> ShowS' -> ShowS'
showParen' b s = if b then showChar '(' . s . showChar ')' else s
showList' :: (Show a)=> [a] -> ShowS'
showList' l = if null l then showString "[]" else showChar '[' . foldr1 (\x s -> x . showChar ',' . s) (map shows l) . showChar ']'
--showsPrec :: (Show a)=> Int -> a -> ShowS
shows' :: (Show a)=> a -> ShowS'
shows' x = (show x ++)

-- Semigroup & Monoid
--(<>) :: (Semigroup a)=> a -> a -> a
--mempty :: (Monoid g)=> g
--mappend :: (Monoid g)=> g -> g -> g
--mconcat :: (Monoid g)=> [g] -> g
--foldMap :: (Foldable l, Monoid g)=> (a -> g) -> l a -> g

-- Traversables
sequence' :: (Monad m)=> [m a] -> m [a] -- sequence :: (Traversable t, Monad m)=> t (m a) -> m (t a)
sequence' = foldr mcons (return [])
  where mcons p q = p >>= \x -> q >>= \y -> return (x:y)
-- sequence' :: [[a]] -> [[a]]
-- sequence' = foldr (\x rec -> [x':xs' | x'<-x, xs'<-rec]) [[]]

-- sequence' [] = [[]]
-- sequence' (xs:xss) = [y:ys | y<-xs, ys<-sequence' xss]

-- sequence' = foldr mcons [[]]
--   where mcons l1 l2 = concat $ fmap (\x -> fmap (\y -> x:y) l2) l1
sequenceA' :: (Applicative f) => [f a] -> f [a] -- sequenceA :: (Traversable t, Applicative f)=> t (f a) -> f (t a)
sequenceA' [] = pure []
sequenceA' (x:xs) = (:) <$> x <*> sequenceA' xs
sequence_' :: (Foldable l, Monad m)=> l (m a) -> m ()
sequence_' = foldr (\p q -> p >> q >> return ()) (return ())
--traverse :: (Traversable t, Applicative f)=> (a -> f b) -> t a -> f (t b)
--mapM :: (Traversable t, Monad m)=> (a -> m b) -> t a -> m (t b)
--mapM_ :: (Foldable l, Monad m)=> (a -> m b) -> t a -> m ()

-- Enumeration Types
--fromEnum :: (Enum a)=> a -> Int
--toEnum :: (Enum a)=> Int -> a
succ' :: Int -> Int -- succ :: (Enum a)=> a -> a
succ' = (+1)
pred' :: Int -> Int -- pred :: (Enum a)=> a -> a
pred' = (+(-1))
-- pred' = (subtract 1)
--enumFrom :: (Enum a)=> a -> a
--enumFromTo :: (Enum a)=> a -> a -> a
--enumFromThen :: (Enum a)=> a -> a -> a
--enumFromThenTo :: (Enum a)=> a -> a -> a -> a
--maxBound, minBound :: (Bounded a)=> a

-- IEEE
--isIEEE, isDenormalized, isInfinite, isNaN, isNegativeZero :: (RealFloat a)=> a -> Bool
--decodeFloat :: (RealFloat a)=> a -> (Integer,Int)
--encodeFloat :: (RealFloat a)=> Integer -> Int -> a
--exponent :: (RealFloat a)=> a -> Int
--floatDigits :: (RealFloat a)=> a -> Int
--floatRadix :: (RealFloat a)=> a -> Integer
--floatRange :: (RealFloat a)=> a -> (Int,Int)
--scaleFloat :: (RealFloat a)=> Int -> a -> a
--significand :: (RealFloat a)=> a -> a
