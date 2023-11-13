-- Vectors and Matrices case study

type Vector = [Int]

vconst :: Int -> Int -> Vector
vconst 0 _ = []
vconst n x = x : vconst (n-1) x

vecAdd :: Vector -> Vector -> Vector
vecAdd = zipWith (+)

vecDot :: Vector -> Vector -> Int
vecDot u v = sum (zipWith (*) u v)

type Matrix = [Vector]

unit :: Int -> Matrix
unit 0 = []
unit n = (1 : vconst (n-1) 0) : (map (0:) (unit (n-1)))

tr :: Matrix -> Matrix
tr [] = []
tr [v] = map (\x -> [x]) v
tr (v:vs) = zipWith (:) v (tr vs)

matAdd :: Matrix -> Matrix -> Matrix
matAdd = zipWith vecAdd

matVecMult :: Matrix -> Vector -> Vector
matVecMult a v = map (`vecDot` v) (tr a)

matMult :: Matrix -> Matrix -> Matrix
matMult a b = map (matVecMult a) b


--------------------------------------------------------------------------------


-- Trees

data Tree t = Leaf | Node t (Tree t) (Tree t)

treefold :: (a -> b -> b -> b) -> b -> Tree a -> b
treefold compute e Leaf = e
treefold compute e (Node x l r) = compute x (treefold compute e l) (treefold compute e r)

treesum :: (Num a) => Tree a -> a
treesum    = treefold (\x l r -> x + l + r)   0

treeleaves :: Tree a -> Int
treeleaves = treefold (\x l r -> 1 + l + r)   0

treedepth :: Tree a -> Int
treedepth  = treefold (\x l r -> 1 + max l r) 0

preorder :: Tree a -> [a]
preorder = treefold (\x l r -> [x] ++ l ++ r) []

inorder :: Tree a -> [a]
inorder = treefold (\x l r -> l ++ [x] ++ r) []

testtree = (Node 1  (Node 10 Leaf Leaf)
                    (Node 17  (Node 14 Leaf Leaf)
                              (Node 20 Leaf Leaf)
                    )
           )


--------------------------------------------------------------------------------


-- Natural number formalism

data Nat = Zero | Succ Nat
  deriving (Eq, Ord, Show)

addNat :: Nat -> Nat -> Nat
addNat n Zero     = n
addNat n (Succ m) = addNat (Succ n) m

multNat :: Nat -> Nat -> Nat
multNat n Zero     = Zero
multNat n (Succ m) = addNat n (multNat n m)

expNat :: Nat -> Nat -> Nat
expNat n Zero     = Succ Zero
expNat n (Succ m) = multNat n (multNat n m)

intToNat :: Int -> Nat
intToNat n = (iterate Succ Zero) !! n

natToInt :: Nat -> Int
natToInt Zero = 0
natToInt (Succ m) = succ (natToInt m)


--------------------------------------------------------------------------------


-- Graham Hutton Compiler Correctness

-- https://www.youtube.com/watch?v=_mHLHP3eH3c&list=PLF1Z-APd9zK5uFc8FKr_di9bfsYv8-lbc&index=14
data Expr = Val Int | Plus Expr Expr -- Source language
  deriving (Show)

eval :: Expr -> Int -- Semantics
eval (Val n) = n
eval (Plus x y) = eval x + eval y

type Stack = [Int] -- Target language; Virtual machine:
type Code = [Op]
data Op = PUSH Int | ADD

exec :: Code -> Stack -> Stack -- Semantics of execution
exec [] s = s
exec (PUSH n : c) s = exec c (n:s)
exec (ADD : c) (m:n:s) = exec c (m+n:s)

comp :: Expr -> Code -- Compiler
comp (Val n) = [PUSH n]
comp (Plus x y) = comp x ++ comp y ++ [ADD]

{-  exec (comp e) [] = eval e  -}

comp' :: Expr -> Code
comp' e = go e []
  where go (Val n) c = PUSH n : c
        go (Plus x y) c = go x (go y (ADD : c))

testexpr = Plus (Plus (Val 2) (Val 3)) (Val 4) -- 9

{-  exec (comp' e c) s = exec c (eval e : s)  -}


--------------------------------------------------------------------------------


-- Monoids

-- newtype Product a = Product { getProduct :: a }
--   deriving (Eq, Ord, Read, Show, Bounded)
-- instance Num a => Monoid (Product a) where
--   mempty = Product 1
--   Product x `mappend` Product y = Product (x * y)

-- newtype Sum a = Sum { getSum :: a }
--   deriving (Eq, Ord, Read, Show, Bounded)
-- instance Num a => Monoid (Sum a) where
--   mempty = Sum 0
--   Sum x `mappend` Sum y = Sum (x + y)

-- newtype All a = All { getAll :: a }
--   deriving (Eq, Ord, Read, Show, Bounded)
-- instance Num a => Monoid (All a) where
--   mempty = All True
--   All x `mappend` All y = All (x && y)

-- newtype Any a = Any { getAny :: a }
--   deriving (Eq, Ord, Read, Show, Bounded)
-- instance Any a => Monoid (Any a) where
--   mempty = Any False
--   Any x `mappend` Any y = Any (x || y)
