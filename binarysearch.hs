search :: (Ord a)=> a -> [a] -> Maybe Int
search x list = seek 0 (length list - 1)
  where
    seek left right
      | left==right = if x==y then Just middle else Nothing
      | otherwise   = case x`compare`y of
        LT -> seek left (middle-1)
        EQ -> Just middle
        GT -> seek (middle+1) right
      where middle = (left+right)`div`2
            y = list !! middle
