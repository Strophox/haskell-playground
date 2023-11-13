-- Nim Game
import System.IO.Unsafe (unsafePerformIO)
dbg :: (Show a)=> a -> a
dbg x = unsafePerformIO (print x >> return x)

data Color = Red | Blue
  deriving (Show)
data Player = User Color | Computer
  deriving (Show)
-- 1 1, 3 1, 5 4, 5 1
type Board = [Int]
type GameState = (Int, Board, [Player])

main = playNim

-- Start playing Nim
playNim :: IO ()
playNim = do
  traverse putStrLn [ ""
    , "Welcome to the Game of Nim!"
    , " • Each turn a player removes some objects from one row."
    , " • The player to take the last object wins."
    , "Available modes to play: Singleplayer / Multiplayer" ]
  putStr "Please choose a gamemode >"
  str <- toLower <$> getLine
  if str`matches`"singleplayer" then do
    putStrLn "...starting game against computer..."
    playNimFrom (1, [1..5], cycle [User Red, Computer])
    main
  else if str`matches`"multiplayer" then do
    putStrLn "...starting session for two users..."
    playNimFrom (1, [1..5], cycle [User Red, User Blue])
    main
  else if str`matches`"computer" then do
    putStrLn "...starting game against computer (goes first)..."
    playNimFrom (1, [1..5], cycle [Computer, User Blue])
    main
  else if str`matches`"test" then do
    putStrLn "Test Start..."
    playNimFrom (1, [1..16], cycle [Computer, Computer])
    putStrLn "Test Completed!"
  else
    putStrLn "Goodbye"
  where
    toLower = map toLowerChar
    toLowerChar c = maybe c id (lookup c (zip ['A'..'Z'] ['a'..'z']))
    matches str1 str2 = all (not.null) [str1,str2] && and (zipWith (==) str1 str2)

-- Play Nim starting from some game position
playNimFrom :: GameState -> IO ()
playNimFrom (i, board, []) = putStrLn "Game Over - No turns left!"
playNimFrom (i, board, player:players) = do
  putStrLn ""
  putStrLn $"ROUND "<>show i<>"."
  putStr (boardToStr board)
  newBoard <- case player of
    User color -> do
      putStr $show color<>" player's turn to enter *row* and *objects to remove*"
      userMove board
    Computer   -> do
      putStrLn "The Computer analyses the board..."
      case bestMove board of
        Just bestBoard -> putStrLn "It found a move!" >> return bestBoard
        Nothing        -> putStrLn "It makes a move!" >> return (rndmMove board)
  if all (0==) newBoard then
    putStrLn $"Game Over - "<>show player<>" won the game!"
  else
    playNimFrom (i+1, newBoard, players)

-- Prettyprint board position to string
boardToStr :: Board -> String
boardToStr board = (unlines . showRows) board
  where
    showRows = map (\(i,o) -> showNum i<>": "<>showRow o) . zip [1..]
    showNum idx = replicate (biggest - length (show idx)) ' ' <> show idx
    biggest = length . show . length $ board
    showRow cnt = replicate (largest - cnt) ' ' <> unwords (replicate cnt "@")
    largest = maximum board

-- Produce the winning move on the board
bestMove :: Board -> Maybe Board
bestMove board = if nimSum /= 0 then Just (fixNimSum board) else Nothing
  where
    fixNimSum (r:rs)
      | r`xor`nimSum < r = (r`xor`nimSum) : rs
      | otherwise        = r : (fixNimSum rs)
    nimSum = foldr xor 0 board
    xor :: Int -> Int -> Int
    xor n m
      | min n m == 0 = max n m
      | otherwise    = 2 * (xor (n`div`2) (m`div`2)) + ((n+m)`mod`2)

-- Modify a row on the board
modifyRow :: (Int,Int) -> Board -> Board
modifyRow (row, cnt) board = (take (row-1) board) ++ [board!!(row-1) - cnt] ++ (drop row board)

-- Allow for move to be made by standard user input
userMove :: Board -> IO Board
userMove board = do
  putStr " >"
  str <- getLine
  case parseNimMove board str of
    Left err        -> putStr err >> userMove board
    Right (row,cnt) -> return (modifyRow (row, cnt) board)

-- Parse valid move from string for a board position or return error message
parseNimMove :: Board -> String -> Either String (Int,Int)
parseNimMove board str
  | any (`notElem`"1234567890 ") str        = Left (
    "Oops! Expected positive integers.")
  | length (words str) /= 2                 = Left (
    "Oops! Expected two integers, separated by space.")
  | not (1 <= row && row <= length board)   = Left (
    "Oops! Invalid row '"<>show row<>"', expected something in the range [1.."<>show (length board)<>"].")
  | board!!(row-1) < 1                      = Left $
    "Row "<>show row<>" is empty, please choose a different one."
  | not (1 <= cnt && cnt <= board!!(row-1)) = Left (
    "Oops! Invalid object count '"<>show cnt<>"' for row "<>show row<>", expected something in the range [1.."<>show (board!!(row-1))<>"]")
  | otherwise = Right (row, cnt)
  where [row, cnt] = map read (words str)

-- Make a pseudorandom move
rndmMove :: Board -> Board
rndmMove board = modifyRow (rndRow,rndCnt) board
  where rotate n xs = let m = n`mod`length xs in drop m xs ++ take m xs
        rndRow = head [row | row <- rotate (1+sum board) (prng (length board)), board!!(row-1) > 0]
        rndCnt = head [cnt | cnt <- rotate (1+sum board) (prng (board!!(rndRow-1)))]

-- Given `n`, return a pseudorandom order for numbers [1..n]
prng :: Int -> [Int]
prng n = filter (<=n) . map (subtract 1 . recipMod p) $ [2..p-1]
  where p = head (filter (>=n+2) primes)

-- All prime numbers
primes :: [Int]
primes = filter prime [2..]
  where prime n = n >= 2 && all ((/=0).(n`mod`)) [2..n-1]

-- Compute multiplicative inverse of x w.r.t. Z_n
recipMod :: Int -> Int -> Int
recipMod n x = if gcd' == 1 then inv`mod`n else error "no inverse"
  where (gcd',_,inv) = euclid n x
--recipMod n x = if gcd n x == 1 then the_inverse else error "no inverse"
--  where the_inverse = head (filter ((==1).(`mod`n).(*x)) [1..n-1])

-- Extended Euclidean algorithm computing the gcd & Bézout coefficients of i,j
euclid :: Int -> Int -> (Int, Int, Int)
euclid i j = extract (until fst_is0 step (i,j, 0,1, 1,0))
  where extract (_,b, x,y, _,_) = (b, x, y)
        fst_is0 (a,_, _,_, _,_) = a==0
        step    (a,b, x,y, u,v) = (r,a, u,v, m,n)
          where (q,r) = divMod b a
                (m,n) = (x-u*q, y-v*q)
