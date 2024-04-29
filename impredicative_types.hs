{-# LANGUAGE ImpredicativeTypes #-}

f :: Num a => (a -> b) -> (forall x. x -> x, b)
f = (\b -> (\x -> x, b 1))

g :: Num a => (a -> b) -> (c -> c, b)
g = (\b -> (\x -> x, b 1))

h :: Num a => ((a -> b) -> (forall x. x -> x, b)) -> (String, Bool)
h fun = let (i, j) = fun (+1)
        in (i "hey", i True)
--h f -- compiles, `("hey", True)`
--h g -- error, `g` not general enough?

-- DOES NOT COMPILE
