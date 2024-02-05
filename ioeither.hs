data IORes e a = IORes { unIORes :: IO (Either e a) }

instance Functor (IORes e) where
-- fmap :: (a -> b) -> IORes e a -> IORes e b
  fmap f = IORes . fmap (fmap f) . unIORes
--fmap f v = IORes (fmap (fmap f) (unIORes v))
--fmap f (IORes x) = IORes (fmap (fmap f) x)

instance Applicative (IORes e) where
-- pure :: a -> IORes e a
  pure = IORes . pure . pure
--pure x = IORes (pure (pure x))

-- (<*>) :: IORes e (a -> b) -> IORes e a -> IORes e b
  u <*> v = IORes ((<*>) <$> unIORes u <*> unIORes v)
--(IORes x) <*> (IORes y) = IORes ((<*>) <$> x <*> y)

instance Monad (IORes e) where
  return = pure
-- (>>=) :: IORes e a -> (a -> IORes e b) -> IORes e b
--"(>>=) :: IO (Either e a) -> (a -> IO (Either e b)) -> IO (Either e b)"
  m >>= f = IORes (unIORes m >>= \x -> case x of
              Left e -> return (Left e); Right a -> unIORes (f a))

-- Embed a value as a successful value or as error
ok :: a -> IORes e a
ok = IORes . pure . Right --ok = pure
err :: e -> IORes e a
err = IORes . pure . Left --err e = IORes (pure (Left e))

-- Turn a normal IO value into a (successful result)
wrap :: IO a -> IORes e a
wrap = IORes . fmap Right --wrap v = IORes (fmap Left v)

-- (Unsafely) unwrap a IORes value into a normal IO value or error otherwise
unwrap :: Show e => IORes e a -> IO a
unwrap v = do
  x <- unIORes v
  case x of
    Left  e -> error ("IORes error, " <> show e)
    Right a -> return a

-- (Safely) unwrap an IORes value into an IO (Either ...) value
resultOf :: IORes e a -> IO (Either e a)
resultOf = unIORes

-- Common IO functions in IORes
putStrLn' :: String -> IORes e ()
putStrLn' = wrap . putStrLn

getChar' :: IORes e Char
getChar' = wrap getChar

-- Handy parser function with proper error feedback
readIORes :: Read a => String -> IORes String a
readIORes str = case reads str of
  [(x,"")] -> pure x
  _ -> err ("no parse: " <> str)

-- HelloWorld in IORes
helloworld :: IORes String ()
helloworld = putStrLn' "hello, world"

test :: String -> IORes String (Bool,Int,String)
test str = do
  wrap $ putStr ("Gonna try parse: "<>str)
  x :: Int <- readIORes str
  putStrLn' (" ...parsed Int = "<>show x)
  if x == 42 then ok (True,x,":-)") else err "D;"

test2 :: IORes String ()
test2 = do
  str <- wrap (putStr "Enter double >" >> getLine)
  x :: Double <- readIORes str
  wrap (putStr "successful parse; ")
  wrap (print x)

main = do
  putStrLn "--TEST helloworld"
  res <- resultOf helloworld
  print res

  putStrLn "--TEST interactive"
  x <- resultOf test2
  putStrLn ("Result of previous test: "<>show x)

  putStrLn "--TEST parse success"
  x <- resultOf $ test "42"
  print x
  x <- unwrap $ test "42"
  print x

  putStrLn "--TEST parse success + manual err"
  x <- resultOf $ test "69"
  print x
  --x <- unwrap $ test "69"
  --print x

  putStrLn "--TEST parse fail"
  x <- resultOf $ test "hi"
  print x
  x <- unwrap $ test "hi"
  print x


{-NOTE unsolvable?
data EitherIO e a = EIO { unEIO :: Either e (IO a) }

instance Functor (EitherIO e) where
  fmap f = EIO . fmap (fmap f) . unEIO

instance Applicative (EitherIO e) where
  pure = EIO . pure . pure
  u <*> v = EIO ((<*>) <$> unEIO u <*> unEIO v)

instance Monad (EitherIO e) where
  return = pure
--TODO (>>=) :: EitherIO e a -> (a -> EitherIO e b) -> EitherIO e b
--TODO faulty attempt-> (EIO x) >>= f = EIO (fmap (>>= f) x)
  (EIO (Left e)) >>= f = (EIO (Left e))
  (EIO (Right x)) >>= f = (EIO (Right (join (fmap f x))) ??!??! !!!?-}
