import Data.Array (listArray,(!),range)

main = do
  putStrLn "Enter two strings to compare (or empty to quit)"
  str1 <- putStr ">" >> getLine
  str2 <- putStr ">" >> getLine
  if null str1 || null str2 then
    return ()
  else do
    print (distance str1 str2)
    main

distance str1 str2 = table ! snd bounds
  where bounds = ((0,0),(length str1, length str2))
        table = listArray bounds (rec <$> range bounds)
        rec (i,j)
          | i<0 || j<0   = 0
          | i==0 || j==0 = i+j
          | otherwise    = minimum
            [ table!(i-1,j)   + 1
            , table!(i,j-1)   + 1
            , table!(i-1,j-1) + (if str1!!(i-1)==str2!!(j-1) then 0 else 1)
            ]
