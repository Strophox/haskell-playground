search :: (Ord a)=> a -> [a] -> Maybe Int
search x list = seek 0 (length list - 1) where
  seek l r
    | l == r = if x == y then Just m else Nothing
    | otherwise   = case x`compare`y of
      LT -> seek l m
      EQ -> Just m
      GT -> seek (m+1) r
    where m = (l+r)`div`2
          y = list !! m
