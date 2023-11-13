import Data.Ratio -- (%)


--------------------------------------------------------------------------------


-- Numerical approximations: Newton's method

newton :: IO ()
newton = do
  a <- putStr"Compute the root of >" >> (maybe (-1) id . readMaybe) <$> getLine
  if a < 0 then return ()
  else do
    putStrLn $ " | Square root = " ++ show (root a)
    newton

root :: Double -> Double
root a = fixpoint (iterate next 1)
  where next x = (x + a/x) / 2
-- root a = until (tolerantOf <*> next) next 1
  -- where next x = (x + a/x) / 2

tolerantOf :: Double -> Double -> Bool
tolerantOf = rel_tol --curry $ (||) <$> uncurry abs_tol <*> uncurry rel_tol
  where
    ε = 1e-6--0.001
    rel_tol x y = abs (x - y) <= ε * abs y
    abs_tol x y = abs (x - y) <= ε

fixpoint :: [Double] -> Double
fixpoint (x:y:zs)
  | x`tolerantOf`y = y
  | otherwise       = fixpoint (y:zs)

infix 5 -?> ; (-?>) :: (Eq a)=> [a] -> [b] -> (a -> Maybe b) -- Produce a total function by attempting a list bijection
as -?> bs = \x -> lookup x (as`zip`bs)

infix 5 -!> ; (-!>) :: (Eq a)=> [a] -> [b] -> (a -> b) -- Produce a partial function by forcing a list bijection
as -!> bs = maybe (error "not in domain") id . (as -?> bs)

readMaybe :: (Read a)=> String -> Maybe a
readMaybe s = case reads s of
  [(x,"")] -> Just x
  _        -> Nothing

-- https://www.youtube.com/watch?v=XrNdvWqxBvA
derivativeApproxs :: (Double -> Double) -> Double -> [Double]
derivativeApproxs f x = {-dbg$-} map approx (iterate (/2) 1)
  where approx h = (f (x+h) - f (x)) / h
--dbg list = unsafePerformIO $ print (take 10 list) >> return list
derive :: (Double -> Double) -> (Double -> Double)
derive f = \x -> fixpoint (derivativeApproxs f x)
derive' f = \x -> fixpoint (improve $ derivativeApproxs f x) -- 2nd order
derive'' f = \x -> fixpoint (improve.improve $ derivativeApproxs f x) -- 3rd order
derive''' f = \x -> fixpoint (improve.improve.improve $ derivativeApproxs f x) -- 4th order
deriveS f = \x -> fixpoint (super $ derivativeApproxs f x)-- ω-th order

testderive = mapM_ (print.run) [0..]
  where run n = let d = [0..]-!>[derive,derive',derive'',derive''',deriveS] $ n in [d (^4) 1, d (^100) 1]

improve :: [Double] -> [Double]
improve sq = {-dbg$-} elimErrorOrder (order sq) sq
  where
    order (x:y:z:_) = round . logBase 2 . max 2 . min (10^10) $ ((x-z) / (y-z) - 1) -- TODO
    elimErrorOrder n (x:y:zs) = let
         x' = (y*2^n - x) / (2^n - 1)
      in x' : elimErrorOrder n (y:zs)

super :: [Double] -> [Double]
super = map (!! 1) . iterate improve
-- within eps (a:b:rest) = if abs(a-b) <= eps then b else within eps (b:rest)
-- relative eps (a:b:rest) = if abs(a-b) <= eps*abs b then b else within eps (b:rest)
-- easydiff f x h = (f(x+h)-f x) / h
-- differentiate h0 f x = map (easydiff f x) (iterate (/2) h0)
-- testdiff = within eps (differentiate h0 f x) where (eps,h0,f,x) = (0.001, 1, (^2), 0)
-- elimerror n (a:b:rest) = (b*(2^n)-a)/(2^n-1) : elimerror n (b:rest)
-- order (a:b:c:rest) = round(logBase 2( (a-c)/(b-c) - 1 ))
-- improve s = elimerror (order s) s
-- testdiff' = within eps (improve (differentiate h0 f x)) where (eps,h0,f,x) = (0.001, 1, (^2), 0)
-- testdiff'' = within eps (improve (improve (improve (differentiate h0 f x)))) where (eps,h0,f,x) = (0.001, 1, (^2), 0)
-- super s = map (!!1) (iterate improve s)
-- testdiff''' = within eps (super (differentiate h0 f x)) where (eps,h0,f,x) = (0.001, 1, (^2), 0)
easyinteg f a b = (f a + f b) * (b-a) / 2
integ f a b = (easyinteg f a b) : zipWith (+) (integ f a m) (integ f m b) where m = (a+b)/2
integ' f a b = {-dbg$-} int f a b (f a) (f b) where int f a b fa fb = let (m, fm) = ((a+b)/2, f m) in ((fa+fb) * (b-a) / 2) : zipWith (+) (int f a m fa fm) (int f m b fm fb)
integrate f = \a b -> fixpoint (integ' f a b)
integrate' f = \a b -> fixpoint (improve $ integ' f a b)
integrateS f = \a b -> fixpoint (super $ integ' f a b)

testinteg = mapM_ (print.run) [0..]
  where run n = let i = [0..]-!>[integrate,integrate',integrateS] $ n in [i (^2) 0 1, i sin 0 4, (*4)$i ((1/).(+1).(^2)) 0 1]


--------------------------------------------------------------------------------


-- Pi in Haskell

-- TOCO https://en.wikipedia.org/wiki/Ramanujan%E2%80%93Sato_series#Similar_series
tauhalf n = realToFrac . sum . take n $ scanl (\a i -> a*(i%(2*i+1))) 2 [1..]

numterms = 1000
evalSeries reduce = (realToFrac . reduce . take numterms)

pi' = 2 * series -- 'Custom'
  where series = evalSeries sum $ scanl (\a i -> a*(i%(2*i+1))) 1 [1..] -- 1 : [ product [1..i] / product [2*j+1|j<-[1..i]] | i <- [1..n] ]

pi'' = sqrt (6 * series) -- Euler
  where series = evalSeries sum $ [ term i | i<-[1..] ]
        term i = 1 / i^2

pi''' = 4 * series -- Leibniz
  where series = evalSeries sum $ 1 : [ term i | i<-[1..] ]
        term i = (if even i then 1 else (-1)) % (2*i+1)

pi'''' = 2 * series -- Wallis
  where series = evalSeries product $ [ term i | i<-[1..] ]
        term i = (4*i^2) % (4*i^2 - 1)
