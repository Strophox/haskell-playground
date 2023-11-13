-- Lambda calculus shite

-- Boolean logic
lT = \t f -> t -- True
lF = \t f -> f -- False
lNOT = \a -> a lF lT -- not
lAND = \a b t f -> a (b t f) f -- (&&)
lOR  = \a b t f -> a t (b t f) -- (||)

-- Tuples
lPAIR = \x y t -> t x y    -- (x, y)
lFST  = \p -> p (\x y -> x) -- fst
lSND  = \p -> p (\x y -> y) -- snd

-- Arithmetic
lS = \n s z -> s (n s z) -- (+1)
l0 = \s z   -> z         -- 0
l1 = lS l0 -- 1
l2 = lS l1 -- 2
l3 = lS l2 -- 3

lADD  = \n m s z -> n s (m s z) -- n + m
lMULT = \n m s z -> n (m s) z   -- n * m
lEXP  = \n m s z -> m n s z     -- n ^ m
lPRE  = \n -> lSND (n (lG lS) (lPAIR l0 l0)) -- (subtract 1)
  where lG = \s p -> lPAIR (s (lFST p)) (lFST p)
-- lSUB  = \n m s z -> n lPRE (m s z)-- n - m -- death

two_lambda = two (+1) 0 where two = \f x -> f (f x)

-- SKI combinator calculus
-- omega = (\x -> x x) (\x -> x x) -- Type error
s :: (z -> a -> b) -> (z -> a) -> z -> b -- s k k == id
s = \x y z -> x z (y z)
k :: a -> b -> a
k = \x y -> x
i :: a -> a
i = \x -> x

two_ski = two succ 0 where two = s (s (k s) k) i  -- s (s (k s) (s (k k) i)) (s (s (k s) (s (k k) i)) (k i))

-- ι = λf.f S K
iota = \f -> ((f (\a -> \b -> \c -> ((a c)(b c)))) (\d -> \e -> d))
{- Doesn't typecheck
two_iota = two succ 0
  where two = s (s (k s) (s (k k) i)) (s (s (k s) (s (k k) i)) (k i))
        i :: a -> a
        i = iota iota
        k :: a -> b -> a
        k = iota (iota (iota iota))
        s :: (z -> a -> b) -> (z -> a) -> z -> b
        s = iota (iota (iota (iota iota)))-}
