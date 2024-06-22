data Bilist a
  = Non
  | a :> Bilist a
  | Bilist a :< a
  deriving Show

bhead :: Bilist a -> Maybe a
bhead Non     = Nothing
bhead (x:>xs) = Just x
bhead (xs:<x) = bhead xs

ends :: Bilist a -> (Maybe a, Maybe a)
ends Non     = (Nothing, Nothing)
ends (x:>xs) = (Just x, (snd . ends) xs)
ends (xs:<x) = ((fst . ends) xs, Just x)

straighten :: Bilist a -> [a]
straighten = go []
  where go acc xs = case xs of
          Non     -> acc
          (x:>xs) -> x : go acc xs
          (xs:<x) -> go (x:acc) xs

testBilist = 0:>(((1:>((2:>(3:>((4:>(5:>Non)):<6))):<7)):<8):<9)
