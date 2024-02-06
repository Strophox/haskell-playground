module Backend where

data Exp = Constant Int
         | Variable String
         | Plus Exp Exp
         | Minus Exp Exp
         | Times Exp Exp
         | Divide Exp Exp
         | Greater Exp Exp
         | Equal Exp Exp
         | Less Exp Exp
         | NotEq Exp Exp
  deriving (Show)

data Com = Assign String Exp
         | Seq Com Com
         | Cond Exp Com Com
         | While Exp Com
         | Declare String Exp Com
         | Print Exp
  deriving (Show)

type Location = Int
type Index = [String]
type Stack = [Int]

position :: String -> Index -> Location
position targetName index = pos 1 index
  where pos n (name:names)
          | name == targetName = n
          | otherwise          = pos (n+1) names

fetch :: Location -> Stack -> Int
fetch n (v:vs) = if n==1 then v else fetch (n-1) vs

put :: Location -> Int -> Stack -> Stack
put n x (v:vs) = if n==1 then x:vs else v : put (n-1) x vs

newtype M a = StOut { unStOut :: (Stack -> (a, Stack, String)) }

instance Functor M where
  fmap f v = StOut $ \s -> let (a,s',str) = unStOut v s in (f a,s',str)

instance Applicative M where
  pure x = StOut $ \s -> (x,s,"")
  u <*> v = StOut $ \s -> let (f,s',str1) = unStOut u s
                              (a,s'',str2) = unStOut v s'
                          in (f a,s'',str1++str2)

instance Monad M where
  return = pure
  m >>= f = StOut $ \s -> let (a,s',str1) = unStOut m s
                              (b,s'',str2) = unStOut (f a) s'
                          in (b,s'',str1++str2)

getFrom :: Location -> M Int
getFrom i = StOut $ \s -> (fetch i s, s, "")

write :: Location -> Int -> M ()
write i v = StOut $ \s -> ((), put i v s, "")

push :: Int -> M ()
push x = StOut $ \s -> ((), x:s, "")

pop :: M ()
pop = StOut $ \(x:s) -> ((), s, "")

output :: Show a => a -> M ()
output v = StOut $ \s -> ((), s, show v)

eval1 :: Exp -> Index -> M Int
eval1 exp index = case exp of
  Constant n -> return n
  Variable x -> getFrom (position x index)
  Plus   x y  -> (+) <$> eval1 x index <*> eval1 y index
  Minus  x y  -> (-) <$> eval1 x index <*> eval1 y index
  Times  x y  -> (*) <$> eval1 x index <*> eval1 y index
  Divide x y  -> div <$> eval1 x index <*> eval1 y index
  Greater x y -> fmap fromEnum . (>) <$> eval1 x index <*> eval1 y index
  Equal   x y -> fmap fromEnum . (==) <$> eval1 x index <*> eval1 y index
  Less   x y -> fmap fromEnum . (<) <$> eval1 x index <*> eval1 y index
  NotEq   x y -> fmap fromEnum . (/=) <$> eval1 x index <*> eval1 y index
{-NOTE original
eval1 exp index = case exp of
  Constant n -> return n
  Variable x -> let loc = position x index in getFrom loc
  Minus x y -> do
    a <- eval1 x index
    b <- eval1 y index
    return (a-b)
  Greater x y -> do
    a <- eval1 x index
    b <- eval1 y index
    return (if a > b then 1 else 0)-}

interpret1 :: Com -> Index -> M ()
interpret1 stmt index = case stmt of
  Assign name e -> do
    v <- eval1 e index
    write (position name index) v
  Seq s1 s2 -> do
    interpret1 s1 index
    interpret1 s2 index
    return ()
  Cond e s1 s2 -> do
    x <- eval1 e index
    if x == 1 then
      interpret1 s1 index
    else
      interpret1 s2 index
  While e b -> let
    loop = do
      v <- eval1 e index
      if v == 0 then
        return ()
      else do
        interpret1 b index
        loop
    in loop
  Declare name e stmt -> do
    v <- eval1 e index
    push v
    interpret1 stmt (name:index)
    pop
  Print e -> do
    v <- eval1 e index
    output v

test x = unStOut (eval1 x []) []

interp x = unStOut (interpret1 x []) []
