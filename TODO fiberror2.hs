--fibMatrix :: (Integral a, Num b)=> a -> b
fibMatrix :: Integer -> Integer
fibMatrix n = (\(_,_,x,_) -> x) (power (0,1,1,1) n)
  where (a,b,c,d) .@ (e,f,g,h) = (a*e+b*g, a*f+b*h, c*e+d*g, c*f+d*h)
        power m n
          | n == 0 = (1,0,0,1)
          | even n = (\h -> h .@ h) $ power m (n`div`2)
          | odd  n = (\h -> m .@ h) $ power m (n-1)

fibMatrix' :: Integer -> Integer
fibMatrix' n = (\(_,_,x,_) -> x) (power id (0,1,1,1) n)
  where (a,b,c,d) .@ (e,f,g,h) = (a*e+b*g, a*f+b*h, c*e+d*g, c*f+d*h)
        power acc m n
          | n == 0 = acc (1,0,0,1)
          | even n = power ((\h -> h .@ h) . acc) m (n`div`2)
          | odd  n = power ((\h -> m .@ h) . acc) m (n-1)
