-- VAMP FS23.1, simplifying function derivatives

data Func
   = Name String   -- (Literal)
   | D   Func      -- Derivative
   | Com Func Func -- Composition
   | Mul Func Func -- Multiplication
   | Add Func Func -- Addition
   deriving (Eq)
instance Num Func where
  (+) f g = Add f g
  (*) f g = Mul f g
  fromInteger int = Name (show int)
  negate f = Mul (fromInteger (-1)) f
  abs f = Mul (signum f) f
  signum f = undefined
infixr 9 .!
(.!) :: Func -> Func -> Func
(.!) f g = Com f g
-- instance Show Func where
--   show func = case func of
--     Name str -> str
--     D f      -> show f ++ "'"
--     Com f g  -> "(" ++ show f ++ " . " ++ show g ++ ")"
--     Mul f g  -> "(" ++ show f ++ " * " ++ show g ++ ")"
--     Add f g  -> "(" ++ show f ++ " + " ++ show g ++ ")"
{- *Main> length.show.testEval $ 1    15    (0.03 secs, 84,480 bytes)
*Main> length.show.testEval $ 2    44    (0.00 secs, 90,176 bytes)
*Main> length.show.testEval $ 3    133    (0.00 secs, 142,712 bytes)
*Main> length.show.testEval $ 9    678387    (0.70 secs, 1,056,184,168 bytes)
*Main> length.show.testEval $ 10    3763059    (3.55 secs, 6,748,695,432 bytes) -}
-- instance Show Func where
--   show f = showFunc f ""
--     where
--       showFunc :: Func -> ShowS
--       showFunc func = case func of
--         Name str -> (str++)
--         D f      -> showFunc f . ('\'':)
--         Com f g  -> ('(':) . showFunc f . (" . "++) . showFunc g . (')':)
--         Mul f g  -> ('(':) . showFunc f . (" * "++) . showFunc g . (')':)
--         Add f g  -> ('(':) . showFunc f . (" + "++) . showFunc g . (')':)
{- *Main> length.show.testEval $ 1    15    (0.01 secs, 76,344 bytes)
*Main> length.show.testEval $ 2    44    (0.01 secs, 87,448 bytes)
*Main> length.show.testEval $ 3    133    (0.01 secs, 118,496 bytes)
*Main> length.show.testEval $ 9    678387    (0.50 secs, 244,284,704 bytes)
*Main> length.show.testEval $ 10    3763059    (2.55 secs, 1,357,685,704 bytes) -}
-- instance Show Function where
--   show = showPrec 0
-- showPrec :: Int -> Func -> String -- Thx: https://stackoverflow.com/questions/20406722/better-display-of-boolean-formulas
-- showPrec prec func
--   | prec > funcPrec = "(" ++ funcStr ++ ")"
--   | otherwise       =        funcStr
--   where
--     funcPrec = case func of
--       Name _  -> 11
--       D _     -> 10
--       Com _ _ -> 9
--       Mul _ _ -> 7
--       Add _ _ -> 6
--     showPrec' = showPrec funcPrec
--     funcStr = case func of
--       Name str -> str
--       D f -> showPrec' f ++ "'"
--       Com f g -> showPrec' f ++ "." ++ showPrec' g
--       Mul f g -> showPrec' f ++ " * " ++ showPrec' g
--       Add f g -> showPrec' f ++ " + " ++ showPrec' g
{- *Main>  length.show.testEval $ 1    9    (0.03 secs, 81,864 bytes)
*Main>  length.show.testEval $ 2    28    (0.00 secs, 86,704 bytes)
*Main>  length.show.testEval $ 3    89    (0.00 secs, 140,832 bytes)
*Main>  length.show.testEval $ 9    483991    (0.59 secs, 843,887,304 bytes)
*Main>  length.show.testEval $ 10    2696939    (3.66 secs, 5,309,502,472 bytes) -}
-- instance Show Function where
--   show f = showPrec 0 f ""
-- showPrec :: Int -> Func -> ShowS -- Thx: https://stackoverflow.com/questions/20406722/better-display-of-boolean-formulas
-- showPrec prec func
--   | prec > funcPrec = ('(':) . funcStr . (')':)
--   | otherwise       =          funcStr
--   where
--     funcPrec = case func of
--       Name _  -> 11
--       D _     -> 10
--       Com _ _ -> 9
--       Mul _ _ -> 7
--       Add _ _ -> 6
--     showPrec' = showPrec funcPrec
--     funcStr = case func of
--       Name str -> (str++)
--       D f -> showPrec' f . ('\'':)
--       Com f g -> showPrec' f . ("."++) . showPrec' g
--       Mul f g -> showPrec' f . (" * "++) . showPrec' g
--       Add f g -> showPrec' f . (" + "++) . showPrec' g
{- *Main> length.show.testEval $ 1    9    (0.02 secs, 82,136 bytes)
*Main> length.show.testEval $ 2    28    (0.00 secs, 87,120 bytes)
*Main> length.show.testEval $ 3    89    (0.00 secs, 130,280 bytes)
*Main> length.show.testEval $ 9    483991    (0.61 secs, 306,458,552 bytes)
*Main> length.show.testEval $ 10    2696939    (3.64 secs, 1,711,268,840 bytes) -}
-- instance Show Func where -- Less readable
--   showsPrec prec func = showParen (prec > prec') funcS
--     where
--       prec' = case func of
--         Name _  -> 11
--         D _     -> 10
--         Com _ _ -> 9
--         Mul _ _ -> 7
--         Add _ _ -> 6
--       showsPrec' = showsPrec prec'
--       funcS = case func of
--         Name str -> showString str
--         D f -> showsPrec' f . showString "'"
--         Com f g -> showsPrec' f . showString "." . showsPrec' g
--         Mul f g -> showsPrec' f . showString " * " . showsPrec' g
--         Add f g -> showsPrec' f . showString " + " . showsPrec' g
instance Show Func where
  showsPrec prec func = showParen (prec > prec') (case func of
      Name s  -> showString s
      D   f   -> sP f . showString "'"
      Com f g -> sP f . showString  "."  . sP g
      Mul f g -> sP f . showString " * " . sP g
      Add f g -> sP f . showString " + " . sP g
    )
    where
      sP = showsPrec prec'
      prec' = case func of
        Name _  -> 11
        D   _   -> 10
        Com _ _ -> 9
        Mul _ _ -> 7
        Add _ _ -> 6

simplify :: Func -> Func
simplify (Name s)  = Name s
simplify (Add f g) = Add (simplify f) (simplify g)
simplify (Mul f g) = Mul (simplify f) (simplify g)
simplify (Com f g) = Com (simplify f) (simplify g)
simplify (D h)     = case h of
  Name s  -> D (Name s)
  Add f g -> simplify $ Add (D f) (D g)
  Mul f g -> simplify $ Add (Mul (D f) g) (Mul f (D g))
  Com f g -> simplify $ Mul (Com (D f) g) (D g)
  D f     -> if simplify (D f) == (D f) then D (D f) else simplify (D (simplify (D f)))

evalStep :: Func -> Func
evalStep (Name a) = Name a
evalStep (Add g f) = Add (evalStep g) (evalStep f) -- Addition
-- evalStep (Mul (Add g f) h) = Add (Mul g h) (Mul f h) -- Distributivity I
-- evalStep (Mul f (Add g h)) = Add (Mul g h) (Mul f h) -- Distributivity II
evalStep (Mul g f) = Mul (evalStep g) (evalStep f) -- Multiplication
evalStep (Com g f) = Com (evalStep g) (evalStep f) -- Composition
evalStep (D (Name a)) = D (Name a)
evalStep (D (Add g f)) = Add (D g) (D f) -- Addition rule
evalStep (D (Mul g f)) = Add (Mul (D g) f) (Mul g (D f)) -- Multiplication rule
evalStep (D (Com g f)) = Mul (Com (D g) f) (D f) -- Chain rule
evalStep (D (D f)) = D (evalStep (D f)) -- Iteration
fullEval :: Func -> Func
fullEval = limit evalStep
limit :: (Eq a)=> (a -> a) -> a -> a
limit f = fst . until (uncurry (==)) ((f>>=(,)) . fst) . (f>>=(,))
  -- where
  --   limit :: (Eq a)=> (a -> a) -> a -> a
  --   limit f = head . dropWhileUnique . iterate f
  --   dropWhileUnique :: (Eq a)=> [a] -> [a]
  --   dropWhileUnique [] = []
  --   dropWhileUnique [x] = [x]
  --   dropWhileUnique (x:y:zs)
  --     | x == y = x:y:zs
  --     | otherwise = dropWhileUnique (y:zs)

(f,g,h) = (Name"f",Name"g",Name"h")
testEval evl' = evl' . (iterate D (f .! g) !!)
testEvalT evl' = traverse (print . length . show . testEval evl') [0..]
