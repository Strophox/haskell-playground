-- Sorting
sortBy :: (a -> a -> Bool) -> [a] -> [a]
sortBy lt [] = []
sortBy lt (p:rest) = sortBy lt [x | x <- rest,      x`lt`p ]
                     ++ [p] ++
                     sortBy lt [y | y <- rest, not (y`lt`p)]

quicksort :: (Ord a)=> [a] -> [a]
-- quicksort [] = [] -- By Lecture 3
-- quicksort (p:rest) = quicksort (filter (<p) rest) ++ [p] ++ quicksort (filter (p<=) rest)
quicksort [] = [] -- By Lecture 3
quicksort (p:rest) = quicksort [x | x<-rest, x < p] ++[p]++ quicksort [y | y<-rest, p <= y]
-- qsort [] = [] -- By Lecture 2
-- qsort (x:xs) = qsort left ++ [x] ++ qsort right
--   where (left, right) = partition (<=x) xs
--         partition p xs = (filter p xs, remove p xs)
--         remove p = filter (not . p)
-- qsort [] = [] -- By Lecture
-- qsort (x:xs) = qsort (lesseq x xs) ++ [x] ++ qsort (greater x xs)
--   where
--     lesseq _ [] = []
--     lesseq x (y:ys)
--       | (y <= x) = y : lesseq x ys
--       | otherwise = lesseq x ys
--     greater _ [] = []
--     greater x (y:ys)
--       | (y > x) = y : greater x ys
--       | otherwise = greater x ys

insertionsort :: (Ord a)=> [a] -> [a]
insertionsort [] = []
insertionsort (x:rest) = insert x (insertionsort rest)
  where insert p list = takeWhile (<p) list ++ [p] ++ dropWhile (<p) list
-- isort [] = []
-- isort (x:xs) = insert x (isort xs)
--   where insert y ys = takeWhile (<y) ys ++ [y] ++ dropWhile (<y) ys
-- inssort [] = [] -- https://youtu.be/I9S61BYM9_4
-- inssort (x:xs) = insert x (inssort xs)
--   where insert x list = let (l,r) = span (<x) list in l++[x]++r

selectionsort :: (Ord a)=> [a] -> [a]
selectionsort [] = []
selectionsort xs = let x = minimum xs in x : selectionsort (xs`without`x)
  where without list p = takeWhile (/=p) list ++ drop 1 (dropWhile (/=p) list)

bubblesort :: (Ord a)=> [a] -> [a]
bubblesort = until noMoreBubbles bubble
  where noMoreBubbles [] = True
        noMoreBubbles [x] = True
        noMoreBubbles (x:y:zs) = (x <= y) && noMoreBubbles (y:zs)
        bubble [] = []
        bubble [x] = [x]
        bubble (x:y:zs) = min x y : bubble (max x y : zs)
-- bubblesort = fst . until (uncurry (==)) (\(b,c)->(c, bubble c)) . (,) []
--   where bubble [] = []
--         bubble [x] = [x]
--         bubble (x:y:zs) = min x y : bubble (max x y : zs)

mergesort :: (Ord a)=> [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort list = merge (mergesort left) (mergesort right)
  where (left,right) = splitAt (length list`div`2) list
        merge (l:ls) (r:rs) = if l<=r then l : merge (ls) (r:rs) else r : merge (l:ls) (rs)
        merge ls [] = ls
        merge [] rs = rs

testsort = let
            totry  = [sortBy (<), quicksort, insertionsort, selectionsort, bubblesort, mergesort]
            input  = [15,32,10,12,9,39,8,26,17,4,13,6,5,34,38,37,41,38,13,26,16,17,15,42,23,23,1,39,8,30,21,29,35,40,32,40,41,39,19,29,36,31]
            output = [1,4,5,6,8,8,9,10,12,13,13,15,15,16,17,17,19,21,23,23,26,26,29,29,30,31,32,32,34,35,36,37,38,38,39,39,39,40,40,41,41,42]
           in map ((==output).($input)) totry
