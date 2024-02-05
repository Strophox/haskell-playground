-- Miscellaneous experiments

-- ghci
-- ghc -o example example.hs
-- ./example

import System.IO.Unsafe (unsafePerformIO) -- OH NO
import Control.Monad
import Control.Arrow
import Control.Category
--import Data.Bool (bool)
--import System.Random -- myConstant = unsafePerformIO randomIO
-- https://downloads.haskell.org/ghc/latest/docs/libraries/
-- :set +s
-- :t (True,'a')
-- :info Num
-- :k ([])
-- :browse GHC.Types


--------------------------------------------------------------------------------


main = putStrLn "hello, world"


--------------------------------------------------------------------------------


testApps = zip [0..] [
  l <* r,
  (const <$> l) <*> r,
  (const id <$> l) <*> r,
  l *> r,
  (id <$ l) <*> r,
  l <**> (const <$> r),
  l <**>? (const <$> r)
  ]
  where
    (l, r) = ([0,4], [1,2,3])
    liftA2 f u v = f <$> u <*> v
    (<**>) = liftA2 (flip ($))
    (<**>?) = flip (<*>)


--------------------------------------------------------------------------------


id' :: (Show a)=> a -> a
id' x = unsafePerformIO $ (putStr . showChar '<' . shows x . showChar '>' $ "") >> return x

fix :: (a -> a) -> a
fix f = let x = f x in x

ones :: [Int]
ones = 1 : ones

naturals :: [Word]
naturals = 0 : map succ naturals

pow :: Int -> Int -> Int
pow _ 0 = 1 -- efficient
pow n e = (if e`mod`2 /= 0 then n else 1) * (pow n (e`div`2))^2
-- pow _ 0 = 1 -- naïve
-- pow n e = n * pow' n (e - 1)

fac :: Integer -> Integer
fac n = product [1..n]

ack :: Integer -> Integer -> Integer -- ack 4 1 would take 3min
ack 0 n = n+1
ack m 0 = ack (m-1) 1
ack m n = ack (m-1) (ack m (n-1))

(??) :: Functor f => f (p -> b) -> p -> f b
f ?? y = fmap ($ y) f
-- f ?? y = \x -> f x y
-- f ?? y = flip f y
-- f ?? y = (`f`y)
-- f ?? y = f <&> ($ y)
-- f ?? y = app . (app . (,) f &&& const y)

readMaybe :: (Read a)=> String -> Maybe a
readMaybe s = case reads s of
  [(x,"")] -> Just x
  _        -> Nothing

chainr :: Foldable t => t (a -> a) -> (a -> a)
chainr = foldr (>>>) id

chainl :: Foldable t => t (a -> a) -> (a -> a)
chainl = foldr (.) id

bindr :: (Foldable t,Monad m) => t (a -> m a) -> a -> m a
bindr = foldr (>=>) return

bindl :: (Foldable t,Monad m) => t (a -> m a) -> a -> m a
bindl = foldr (<=<) return

compr :: (Foldable t,Arrow a) => t (a b b) -> a b b
compr = foldr (>>>) returnA

compl :: (Foldable t,Arrow a) => t (a b b) -> a b b
compl = foldl (<<<) returnA

testcompr = compr [(*2), (*3), (*5)] 1
testcompr' = runKleisli (compr [Kleisli (\b -> [0:b,1:b]), Kleisli (\b -> [0:b,1:b])]) []

-- Record syntax
data Person = Instance { name :: String }

-- IO interact
padIOErroneous :: IO ()
padIOErroneous = interact (changeLinesWith pad)
  where changeLinesWith f = unlines . map (\line -> f line) . lines
        pad = drop 1 . concatMap (\c -> ['-',c])

padIO :: IO ()
padIO = interact (changeLinesWith pad)
  where changeLinesWith f = unlines . map (\line -> last line`seq`f line) . lines
        pad = drop 1 . concatMap (\c -> ['-',c])

invertIO :: IO ()
invertIO = interact (changeLinesWith invert)
  where changeLinesWith f = unlines . map (\line -> last line`seq`f line) . lines
        invert = map invertChar
        invertChar c = if c`elem`['A'..'Z']++['a'..'z'] then invertLetter c else c
        invertLetter = toEnum . (+ 97) . (25-) . (subtract 97) . fromEnum

-- https://youtu.be/O1-ruHzabAU
-- reduce :: Eq a=> [[a]] -> [[a]]
-- reduce list = map (\cs -> if cs`elem`fixed then cs else filter (`notElem`concat fixed) cs) list
--   where fixed = [cs | cs<-list, length cs == 1]
-- testreduce = reduce ["1234", "1", "34", "3"] == ["24", "1", "4", "3"]

-- Produce a total function by attempting a list bijection
infix 5 -?>
(-?>) :: (Eq a)=> [a] -> [b] -> (a -> Maybe b)
xs -?> ys = \x -> lookup x (xs`zip`ys)

-- Produce a partial function by forcing a list bijection
infix 5 -!>
(-!>) :: (Eq a)=> [a] -> [b] -> (a -> b)
xs -!> ys = (zip xs ys !>)
--as -!> bs = maybe (error "not in domain") id . (as -?> bs)

-- Lookup in an assoc list and error otherwise
(!>) :: (Eq a)=> [(a,b)] -> a -> b
xys !> x = maybe (error "not in assoc list") id (lookup x xys)



{-data Mapping a b = Assoc [(a,b)] | Fun (a -> b)

instance Category Mapping where
  id = Fun id
  (.) =-}


--------------------------------------------------------------------------------

{-
fix :: (a -> a) -> a
fix f = let x = f x in x
-- fix f = f (fix f) -- Less efficient: compare fix (1:) !! (10^8)
-- fix f = foldr (\_->f) undefined (repeat undefined)

fixAssoc :: [(a, [(a, b -> c)] -> b -> c)] -> [(a, b -> c)]
fixAssoc cs = map (fmap ($ fixAssoc cs)) cs
-}

loeb :: Functor f => f (f b -> b) -> f b
loeb x = fix $ \xs -> fmap ($ xs) x
--loeb x = go where go = fmap ($ go) x
--loeb x = fmap ($ loeb x) x

loebM :: (Traversable f, MonadFix m) => f (f b -> m b) -> m (f b)
loebM x = mfix $ \xs -> traverse ($ xs) x

moeb :: (((a -> b) -> b) -> c -> a) -> c -> a
moeb f x = go where go = f ($ go) x


--------------------------------------------------------------------------------


-- Interesting set operations

-- powerset :: [a] -> [[a]] -- All sublists of a list of elements
-- powerset [] = [[]]
-- powerset (x:xs) = (powerset xs) ++ map (x:) (powerset xs)
-- powerset :: [a] -> [[a]]
-- powerset = foldr (\e sets -> [s | set <- sets, s <- [set, e:set]]) [[]]
powerset :: [a] -> [[a]]
powerset = foldr (\e sets -> sets ++ map (e:) sets) [[]]
-- powerset :: [a] -> [[a]]
-- powerset = foldl (\sets e -> sets ++ map (e:) sets) [[]]
-- powerset :: [a] -> [[a]]
-- powerset [] = [[]]
-- powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs
-- powerset :: [a] -> [[a]] -- By Someone
-- powerset xs = filterM (const [True, False]) xs

wordsN :: [a] -> Int -> [[a]] -- Language of all words of length n
wordsN alphabet n = sequence (replicate n alphabet)
-- wordsL :: Alphabet a -> Int -> [Word a]
-- wordsL sigma n = foldr (\_ wrds -> [c:cs | c <- sigma, cs <- wrds]) [[]] [1..n]
-- wordsL :: [a] -> Int -> [[a]]
-- wordsL  = (sequence .) . (flip replicate)
-- wordsOfLength :: Alphabet a -> Int -> [Word a]
-- wordsOfLength 0 sigma = [[]]
-- wordsOfLength n sigma = [ (l:ls) | l<-sigma, ls <- wordsOfLength (n-1) sigma ]

kleeneStar :: [a] -> [[a]] -- Complete language
kleeneStar alphabet = [word | n<-[0..], word <- wordsN alphabet n]

combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = map (x:) (combinations (n-1) xs) ++ combinations n xs


--------------------------------------------------------------------------------


-- Miscellaneous varia

_' = ()
abc = (head .) . zip -- \(x:_) (y:_)->(x,y)
bcd = map ($ 1) [ (+4), (\x->3*x-1).(2^) ] -- 5 2 2
cde = ($ 2) sqrt -- sqrt 2
--def = (thing, length thing) where thing = nub . map product . powerset $ [2,2,2,3,3,3,5,7,11] -- https://oeis.org/A000005/b000005.txt
efg = (,) 1 2
fgh = (,,) 2 3 4
ghi = (,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,) 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62
hij = sequence [("hey.",3), ("ho.",4)]
ijk = sequence [Just 1, Just 2]
jkl = head . return -- id
klm = error "404"
lmn = fail "epic fail" :: Maybe String
mno = \[] -> "hi"
nop = (1+2+)
opq = let 2 + 2 = 5 in 2 + 2
pqr = putStrLn <$> ((++) <$> getLine <*> getLine)
qrs = fmap (+1) (+2) 0
rst = [1,2,4] < [1,2,5]
stu = ($ 3) <$> (*) <$> [1,2,3]
tuv = (<$>) ($ "x") $ (<$>) id $ foldr (((<$>) (++).).(<*>)) [id, (++"0")] [[($ "a")],[($ "b")]]
uvw = ((flip (:) .) . flip (:) . (:[])) 1 2 3
vwx = 'a' <$ (111,259)
wxy = ((*) . (+4) . (+10)) 1 1
xyz = fix error

list01 = [ 1 | True ]
let01 = let {} in 2
const01 = (const . const . const . const) 0 1 2 3 4
testG | even 5 = 6 | odd 5 = 42
appl = let (===) a b c = a == b && b == c
           f = (===) <$> (+2) <*> (*2) <*> (^2)
       in (f 1, f 2)
ho_poly (id :: forall a. a -> a) = (id 7, id True) -- Requires type annot


--------------------------------------------------------------------------------


-- Rewriting everything as foldr
{-
foldr op a [] = []
foldr op a (x:xs) = op x (foldr op a xs)

foldl op a xs = h xs a
  where h [] a = a
        h (x:xs) a = h xs (op a x)

g [] = b               <==>  g = foldr f b
g (x:xs) = f x (g xs)
-}

{-
-- foldl op a [] = a
-- foldl op a (x:xs) = foldl op (op a x) xs
foldl op a
foldl op a = h
  where h [] = \a -> a
        h (x:xs) = (\x' fun a -> fun (op a x')) x (h xs)

a = a
f = \x' f a -> f (op a x')
-}

foldl'f :: (b -> a -> b) -> b -> [a] -> b
foldl'f op a xs = foldr (\x' fun a -> fun (op a x')) (\a -> a) xs a

{-
-- scanl op a [] = [a]
-- scanl op a (x:xs) = a : (scanl' op (op a x) xs)
scanl op a xs = h xs a
  where h [] a = [a]
        h (x:xs) a =  a : (h xs (op a x))
scanl op a xs = h xs a
  where h [] = \a -> [a]
        h (x:xs) = (\x' fun a -> a : (fun (op a x'))) x (h xs)
-}

scanl'f :: (b -> a -> b) -> b -> [a] -> [b]
scanl'f op a xs = foldr (\x' fun a -> a : (fun (op a x'))) (\a -> [a]) xs a


--------------------------------------------------------------------------------


-- Precedences and fixities of prelude operators

{-
|-------|-----------------|----------------|------------------|
| Prec. | Leftassociative | Nonassociative | Rightassociative |
|-------|-----------------|----------------|------------------|
|   9   | !!              |                | .                |
|-------|-----------------|----------------|------------------|
|   8   |                 |                | ^  ^^  **        |
|-------|-----------------|----------------|------------------|
|   7   | * /  `div` `mod`|                |                  |
|       |     `quot` `rem`|                |                  |
|-------|-----------------|----------------|------------------|
|   6   | + -             |                |                  |
|-------|-----------------|----------------|------------------|
|   5   |                 |                | :  ++            |
|-------|-----------------|----------------|------------------|
|   4   | <*>  <$>        | == /= < <= > >=|                  |
|       |                 |`elem` `notElem`|                  |
|-------|-----------------|----------------|------------------|
|   3   |                 |                | &&               |
|-------|-----------------|----------------|------------------|
|   2   |                 |                | ||               |
|-------|-----------------|----------------|------------------|
|   1   | >>  >>=         |                | =<<              |
|-------|-----------------|----------------|------------------|
|   0   |                 |                | $  $! `seq`      |
|-------|-----------------|----------------|------------------|
-}

-- Making a custom operator
{-
Can be used:
  ! # $ % & * + . / < = > ? @ \ ^ | - ~
Can be used after the first symbol:
  :
Can't be used: -- TOCO fix, I could actually use '|' wtf
  ( ) , ; [ ] ` { } _ " '
Reserved op:
  .. : :: = \ | <- -> @ ~ =>
-}

-- All 255 Prelude items
{-
!! $ $! && * ** *> + ++ - . / /= < <$ <$> <* <*> <= <> =<< == > >= >> >>=
Applicative Bool Bounded Char Double EQ Either Enum Eq False FilePath Float
Floating Foldable Fractional Functor GT IO IOError Int Integer Integral Just LT
Left Maybe Monad Monoid Nothing Num Ord Ordering Rational Read ReadS Real
RealFloat RealFrac Right Semigroup Show ShowS String Traversable True Word ^ ^^
abs acos acosh all and any appendFile asTypeOf asin asinh atan atan2 atanh break
ceiling compare concat concatMap const cos cosh curry cycle decodeFloat div
divMod drop dropWhile either elem encodeFloat enumFrom enumFromThen
enumFromThenTo enumFromTo error errorWithoutStackTrace even exp exponent fail
filter flip floatDigits floatRadix floatRange floor fmap foldMap foldl foldl1
foldr foldr1 fromEnum fromInteger fromIntegral fromRational fst gcd getChar
getContents getLine head id init interact ioError isDenormalized isIEEE
isInfinite isNaN isNegativeZero iterate last lcm length lex lines log logBase
lookup map mapM mapM_ mappend max maxBound maximum maybe mconcat mempty min
minBound minimum mod negate not notElem null odd or otherwise pi pred print
product properFraction pure putChar putStr putStrLn quot quotRem read readFile
readIO readList readLn readParen reads readsPrec realToFrac recip rem repeat
replicate return reverse round scaleFloat scanl scanl1 scanr scanr1 seq sequence
sequenceA sequence_ show showChar showList showParen showString shows showsPrec
significand signum sin sinh snd span splitAt sqrt subtract succ sum tail take
takeWhile tan tanh toEnum toInteger toRational traverse truncate uncurry
undefined unlines until unwords unzip unzip3 userError words writeFile zip zip3
zipWith zipWith3 ||
-}


-- Playground: -----------------------------------------------------------------
