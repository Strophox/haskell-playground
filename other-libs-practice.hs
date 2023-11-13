
-- Practice implementations for some functions:
-- CONTROL.MONAD -- https://downloads.haskell.org/ghc/latest/docs/libraries/base-4.18.0.0/Control-Monad.html

(<$!>) f x = x `seq` (x >>= (return . f))
infixl 4 $>
($>) :: Functor f => f a -> b -> f b
($>) = flip (<$)
infixl 1 <&>
(<&>) :: Functor f => f a -> (a -> b) -> f b
(<&>) = flip (<$>)
infixl 4 <$!> -- https://downloads.haskell.org/ghc/latest/docs/libraries/base-4.18.0.0/Control-Monad.html
(<$!>) :: Monad m => (a -> b) -> m a -> m b -- Strict version of <$>.
liftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2 f x y = f <$> x <*> y
-- liftA2 f x = (<*>) (fmap f x)
liftA3 :: Applicative f => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
liftA3 f x y z = f <$> x <*> y <*> z
infixl 4 <**>
(<**>) :: Applicative f => f a -> f (a -> b) -> f b
(<**>) = flip (<*>)
when :: Applicative f => Bool -> f () -> f ()
when b f = if b then f else pure ()
join :: Monad m => m (m a) -> m a -- TODO understand
join = (>>= id)
-- join bss = do bs <- bss ; bs
--ap :: Monad m => m (a -> b) -> m a -> m b
--liftM2 :: Monad m => (a1 -> a2 -> r) -> m a1 -> m a2 -> m r
--forM :: (Traversable t, Monad m) => t a -> (a -> m b) -> m (t b)
--forM_ :: (Foldable t, Monad m) => t a -> (a -> m b) -> m ()
--(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
--forever :: Applicative f => f a -> f b
--void :: Functor f => f a -> f ()
--foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b
--foldM_ :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m ()
--guard :: Alternative f => Bool -> f ()
on :: (b -> b -> c) -> (a -> b) -> (a -> a -> c)
on op f = \x y -> f x `op` f y
-- data Void
-- absurd :: Void -> a
-- absurd _' = case _' of {}
-- vacuous :: Functor f => f Void -> f a
-- vacuous = fmap absurd


--------------------------------------------------------------------------------


-- Practice implementations for some functions:
-- DATA.LIST -- https://hackage.haskell.org/package/base-4.18.0.0/docs/Data-List.html

-- Basic functions
--(++) :: [a] -> [a] -> [a]
--head :: [a] -> a
--last :: [a] -> a
--tail :: [a] -> [a]
--init :: [a] -> [a]
uncons :: [a] -> Maybe (a, [a])
uncons []     = Nothing
uncons (x:xs) = Just (x, xs)
singleton :: a -> [a]
singleton x = [x]
--null :: (Foldable l)=> l a -> Bool
--length :: (Foldable l)=> l a -> Int

-- List transformations
--map :: (a -> b) -> [a] -> [b]
--reverse :: [a] -> [a]
intersperse :: a -> [a] -> [a]
intersperse x xs = drop 1 (concatMap (\x' -> [x,x']) xs)
intercalate :: [a] -> [[a]] -> [a]
intercalate xs xss = concat (intersperse xs xss)
-- intercalate xs xss = drop (length xs) (concatMap (\xs' -> xs++xs') xss)
transpose :: [[a]] -> [[a]]
transpose [] = []
transpose [xs] = map (:[]) xs
transpose (xs:xss) = zipWith (:) xs (transpose xss)
subsequences :: [a] -> [[a]] -- TODO make work on finite lists
subsequences [] = [[]]
subsequences (x:xs) = concat $ zipWith listuple (subsequences xs) ((x:) <$> subsequences xs)
  where listuple x y = [x,y] -- flip (flip (:) . (:[]))
permutations :: [a] -> [[a]] -- https://stackoverflow.com/questions/24484348/what-does-this-list-permutations-implementation-in-haskell-exactly-do/24564307#24564307
permutations [] = [[]]
permutations l = [p | (x, xs) <- pick l, p <- map (x:) (permutations xs)]
  where
    pick :: [a] -> [(a, [a])] -- All ways to take an element out of the list
    pick [x] = [(x, [])]
    pick (x:xs) = (x, xs) : [(y, x:ys) | (y, ys) <- pick xs]
    -- pick xs = [extract n xs | n <- [1..length xs]]
    --   where extract n list = let (a, b) = splitAt n list in (last a, init a ++ b)

    -- pick xs = [extract n xs | n <- [1..length xs]]
    --   where extract n list = (\(a,b) -> (last a, init a ++ b)) (splitAt n list)

    -- pick [x] = [(x,[])]
    -- pick (x:xs) = (x,xs) : map (\(y,ys) -> (y, x:ys)) (pick xs)

-- Reducing lists
--foldl :: (Foldable l)=> (b -> a -> b) -> b -> l a -> b
foldl' :: (b -> a -> b) -> b -> [a] -> b -- Can evaluate foldl' (+) 0 [1..10^7]
foldl' f z []     = z
foldl' f z (x:xs) = let z' = z `f` x in seq z' $ foldl' f z' xs
--foldl1 :: (Foldable l)=> (a -> a -> a) -> l a -> a
--foldl1' :: (a -> a -> a) -> [a] -> a
--foldr :: (Foldable l)=> (a -> b -> b) -> b -> l a -> b
--foldr1 :: (Foldable l)=> (a -> b)
--concat :: (Foldable l)=> l [a] -> [a]
--concatMap :: (Foldable l)=> (a -> [b]) -> l a -> [b]
--and :: (Foldable l)=> l Bool -> Bool
--or :: (Foldable l)=> l Bool -> Bool
--any :: (Foldable l)=> (a -> Bool) -> l a -> Bool
--all :: (Foldable l)=> (a -> Bool) -> l a -> Bool
--sum :: (Foldable l, Num a)=> l a -> a
--product :: (Foldable l, Num a)=> l a -> a
-- maximum :: (Foldable l, Ord a)=> l a -> a
-- minimum :: (Foldable l, Ord a)=> l a -> a

-- Building lists
--scanl :: (b -> a -> b) -> b -> [a] -> [b]
--scanl' :: (b -> a -> b) -> b -> [a] -> [b]
--scanl1 :: (a -> a -> a) -> [a] -> [a]
--scanr :: (a -> b -> b) -> b -> [a] -> [b]
--scanr' :: (a -> b -> b) -> b -> [a] -> [b]
--scanr1 :: (a -> a -> a) -> [a] -> [a]
--mapAccumL :: (Traversable t)=> (s -> a -> (s, b)) -> s -> t a -> (s, t b)
--mapAccumR :: (Traversable t)=> (s -> a -> (s, b)) -> s -> t a -> (s, t b)
--iterate :: (a -> a) -> a -> [a]
--iterate' :: (a -> a) -> a -> [a]
--repeat :: a -> [a]
--replicate :: Int -> a -> [a]
--cycle :: [a] -> [a]
unfoldr :: (b -> Maybe (a,b)) -> b -> [a] -- unfoldr f' (foldr f z xs) == xs <==> f' (f x y) = Just (x,y) && f' z = Nothing
unfoldr f' seed = case f' seed of
  Just (x, seed') -> x : unfoldr f' seed'
  Nothing -> []

-- Sublists
--take :: Int -> [a] -> [a]
--drop :: Int -> [a] -> []
--splitAt :: Int -> [a] -> ([a],[a])
--takeWhile :: (a -> Bool) -> [a] -> [a]
--dropWhile :: (a -> Bool) -> [a] -> [a]
--span :: (a -> Bool) -> [a] -> ([a],[a])
--break :: (a -> Bool) -> [a] -> ([a],[a])
stripPrefix :: (Eq a)=> [a] -> [a] -> Maybe [a]
stripPrefix p l
  | p`isPrefixOf`l = Just (drop (length p) l) -- hack
  | otherwise = Nothing
group :: (Eq a) => [a] -> [[a]]
group [] = []
group (x:[]) = [[x]]
group (x:y:zs)
  | x /= y    = [x] : restgroups
  | otherwise = (x : head restgroups) : tail restgroups
  where restgroups = group (y:zs)
inits :: [a] -> [[a]]
inits [] = [[]]
inits xs = inits (init xs) ++ [xs]
--inits [] = [[]]
--inits (x:xs) = [] : map (x:) (inits xs)
tails :: [a] -> [[a]]
tails [] = [[]]
tails xs = xs : tails (tail xs)
--tails = map reverse . reverse . inits . reverse -- hack
isPrefixOf :: (Eq a)=> [a] -> [a] -> Bool
isPrefixOf p l = and (zipWith (==) p l)
isSuffixOf :: (Eq a)=> [a] -> [a] -> Bool
isSuffixOf s l = reverse s`isPrefixOf`reverse l -- hack
isInfixOf :: (Eq a)=> [a] -> [a] -> Bool
isInfixOf i l = any (i`isPrefixOf`) (tails l)
isSubsequenceOf :: (Eq a)=> [a] -> [a] -> Bool
isSubsequenceOf [] [] = True
isSubsequenceOf ss [] = False
isSubsequenceOf [] ts = True
isSubsequenceOf (s:ss) (t:ts) = s==t && (ss`isSubsequenceOf`ts) || (s:ss)`isSubsequenceOf`ts

-- Searching lists
--elem :: (Foldable l, Eq a)=> a -> l a -> Bool
--notElem :: (Foldable l, Eq a)=> a -> l a -> Bool
--lookup :: (Eq a)=> a -> [(a,b)] -> Maybe b
find :: (a -> Bool) -> [a] -> Maybe a -- find :: (Foldable l)=> (a -> Bool) -> l a -> Maybe a
find check = lookup True . map (check >>= (,))
-- find check [] = Nothing
-- find check (x:xs)
--   | check x   = Just x
--   | otherwise = find check xs
-- find check = lookup True . map (\x -> (check x, x))
--filter :: (a -> Bool) -> [a] -> [a]
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p l = (filter p l, filter (not.p) l)

-- Indexing lists
--(!!) :: [a] -> Int -> a
elemIndex :: (Eq a)=> a -> [a] -> Maybe Int
elemIndex e [] = Nothing
elemIndex e (x:xs)
  | e == x    = Just 0
  | otherwise = (+1) <$> (elemIndex e xs)
elemIndices :: (Eq a)=> a -> [a] -> [Int]
elemIndices e l = case elemIndex e l of
  Just n  -> n : (map (n+1+) . elemIndices e . drop (n+1)) l
  Nothing -> []
findIndex :: (a -> Bool) -> [a] -> Maybe Int
findIndex p [] = Nothing
findIndex p (x:xs)
  | p x       = Just 0
  | otherwise = (+1) <$> (findIndex p xs)
findIndices :: (a -> Bool) -> [a] -> [Int]
findIndices p l = case findIndex p l of
  Just n  -> n : (map (n+1+) . findIndices p . drop (n+1)) l
  Nothing -> []

-- Zipping and unzipping lists
--zip :: [a] -> [b] -> [(a,b)]
--zip3 :: [a] -> [b] -> [c] -> [(a,b,c)]
--zip4 :: [a] -> [b] -> [c] -> [d] -> [(a,b,c,d)]
--zip5 :: [a] -> [b] -> [c] -> [d] -> [e] -> [(a,b,c,d,e)]
--zip6 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] [(a,b,c,d,e,f)]
--zip7 :: [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g] -> [(a,b,c,d,e,f,g)]
--zipWith (a -> b -> c) -> [a] -> [b] -> [c]
--zipWith3 (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
--zipWith4 (a -> b -> c -> d -> e) -> [a] -> [b] -> [c] -> [d] -> [e]
--zipWith5 (a -> b -> c -> d -> e -> f) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f]
--zipWith6 (a -> b -> c -> d -> e -> f -> g) -> [a] -> [b] -> [c] -> [d] -> [e] -> [f] -> [g]
--zipWith7 (a -> b -> c -> d -> e -> f -> g -> h) -> [a] -> [b] -> [c]] -> [d] -> [e] -> [f] -> [g] -> [h]
--unzip :: [(a,b)] -> ([a],[b])
--unzip3 :: [(a,b,c)] -> ([a],[b],[c])
--unzip4 :: [(a,b,c,d)] -> ([a],[b],[c],[d])
--unzip5 :: [(a,b,c,d,e)] -> ([a],[b],[c],[d],[e])
--unzip6 :: [(a,b,c,d,e,f)] -> ([a],[b],[c],[d],[e],[f])
--unzip7 :: [(a,b,c,d,e,f,g)] -> ([a],[b],[c],[d],[e],[f],[g])

-- Special lists
--lines :: String -> [String]
--words :: String -> [String]
--unlines :: [String] -> String
--unwords :: [String] -> String
nub :: (Eq a)=> [a] -> [a] -- (The name nub means `essence'.)
nub [] = []
nub [x] = [x]
nub (x:y:zs) = if x/=y then (x:y:zs) else (y:zs)
delete :: (Eq a)=> a -> [a] -> [a]
delete e [] = []
delete e (x:xs)
  | e == x    = xs
  | otherwise = x : delete e xs
infix 5 \\ ; (\\) :: (Eq a)=> [a] -> [a] -> [a] -- (xs ++ ys) \\ xs == ys
[] \\ [] = []
[] \\ ys = []
xs \\ [] = xs
(x:xs) \\ (y:ys)
  | x == y    = xs \\ ys
  | otherwise = x : (xs \\ (y:ys))
testremove = "Hello World!" \\ "ell W" -- "Hoorld!"
union :: (Eq a)=> [a] -> [a] -> [a]
union xs ys = xs ++ filter (`notElem`xs) ys
intersect :: (Eq a)=> [a] -> [a] -> [a]
intersect xs ys = filter (`elem`ys) xs
sort :: (Ord a)=> [a] -> [a]
sort [] = []
sort (x:xs) = let (left,right) = partition (<x) xs in sort left++[x]++sort right
--sortOn :: (Ord b)=> (a -> b) -> [a] -> [a]
insert :: (Ord a)=> a -> [a] -> [a]
insert e [] = [e]
insert e (x:xs)
  | e <= x    = e : x : xs
  | otherwise = x : insert e xs

-- Generalized functions
--nubBy :: (a -> a -> Bool) -> [a] -> [a]
--deleteBy :: (a -> a -> Bool) -> a -> [a] -> [a]
--deleteFirstsBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
--unionBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
--intersectBy :: (a -> a -> Bool) -> [a] -> [a] -> [a]
--groupBy :: (a -> a -> Bool) -> [a] -> [[a]]
--sortBy :: (a -> a -> Ordering) -> [a] -> [a]
--insertBy :: (a -> a -> Bool) -> a -> [a] -> [a]
--maximumBy :: (Foldable l)=> (a -> a -> Bool) -> l a -> a
--minimumBy :: (Foldable l)=> (a -> a -> Bool) -> l a -> a
--genericLength :: (Num i)=> [a] -> i
--genericTake :: (Num i)=> i -> [a] -> [a]
--genericDrop :: (Num i)=> i -> [a] -> [a]
--genericSplitAt :: (Num i)=> i -> [a] -> ([a],[a])
--genericIndex :: (Num i)=> [a] -> i -> a
--genericReplicate :: (Num i)=> i -> a -> [a]


--------------------------------------------------------------------------------


-- Practice implementations for some functions:
-- https://hackage.haskell.org/package/ilist-0.4.0.1/docs/Data-List-Index.html
indexed :: [a] -> [(Int, a)]
indexed = zip [0..]

deleteAt :: Int -> [a] -> [a] -- If the index is negative or exceeds list length, the original list will be returned.
deleteAt i xs = take i xs ++ drop (i+1) xs

setAt :: Int -> a ->[a] -> [a] -- FIXME If the index is negative or exceeds list length, the original list will be returned.
setAt i x xs = take i xs ++ [x] ++ drop (i+1) xs
s
modifyAt :: Int -> (a -> a) -> [a] -> [a] -- FIXME If the index is negative or exceeds list length, the original list will be returned.
modifyAt i f xs = take i xs ++ [f (xs!!i)] ++ drop (i+1) xs

updateAt :: Int -> (a -> Maybe a) -> [a] -> [a] -- FIXME If the index is negative or exceeds list length, the original list will be returned.
updateAt i m xs = take i xs ++ (case m (xs!!i) of Nothing -> [] ; Just x -> [x]) ++ drop (i+1) xs

insertAt :: Int -> a -> [a] -> [a] -- FIXME If the index is negative or exceeds list length, the original list will be returned. (If the index is equal to the list length, the insertion can be carried out.)
insertAt i x xs = take i xs ++ (if i<=length xs then [x] else []) ++ drop i xs
