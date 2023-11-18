import Data.List
import Data.Maybe (isNothing,catMaybes)
--import Debug.Trace (traceShowId)

data Mark = X | O
  deriving (Show,Eq)

-- "Let game positions be represented by objects of the type [Position]."
type Position = [Maybe Mark]

emptyP :: Position
emptyP = replicate 9 Nothing

{-showP :: Position -> String
showP board = "|"<>concatMap showM board<>"|"
  where showM (Just m) = show m
        showM Nothing  = "-"-}

getPathVals :: Position -> [(Int,Int)]
getPathVals board = map (count . map (board!!)) pathIndices
  where
    count r = (length (filter (==Just X) r), length (filter (==Just O) r))
    pathIndices = let rs = [[0,1,2],[3,4,5],[6,7,8]] in
      [0,4,8] : [2,4,6] : transpose rs ++ rs

player :: Position -> Mark
player board = if length (catMaybes board)`mod`2 == 0 then X else O

-- "There must be some way of knowing what moves can be made from a position"
moves :: Position -> [Position]
moves board = if any (`elem`[(3,0),(0,3)]) (getPathVals board)
  then []
  else [putMark (player board) i board | i<-[0..8], isNothing (board !! i)]
  where
    putMark :: Mark -> Int -> Position -> Position
    putMark mk coord board = left ++ [Just mk] ++ tail right
      where (left,right) = splitAt coord board

-- "the datatype of ordered labeled trees"
data Tree a = Node a [Tree a]

instance Functor Tree where
  fmap f (Node x ns) = Node (f x) (map (fmap f) ns)

iterateTree :: (a -> [a]) -> a -> Tree a
iterateTree f x = Node x (map (iterateTree f) (f x))

-- "constructs a game tree from a particular position"
gametree :: Position -> Tree Position
gametree p = iterateTree moves p

-- "The result of the static evaluation is a measure of the promise of a position from the computer’s point of view (assuming that the computer is playing the game against a human opponent) The larger the result, the better the position for the computer."
static :: Position -> Int
static board
  | has (3,0) = maxBound
  | has (0,3) = minBound
  | has (2,0) = if turn then maxBound else 2
  | has (0,2) = if turn then -2 else minBound
  | has (1,0) = if turn then 1 else 0
  | has (0,1) = if turn then 0 else -1
  | otherwise = 0
  where
    has  = (`elem` getPathVals board)
    turn = player board == X

-- "Assuming that the computer and its opponent alternate turns, the true value of a node is computed by the function maximize if it is the computer’s turn and minimize if it is not"
maximize :: Tree Int -> Int
maximize (Node x []) = x
maximize (Node x ns) = maximum (map minimize ns)

minimize :: Tree Int -> Int
minimize (Node x []) = x
minimize (Node x ns) = minimum (map maximize ns)

-- "The function (prune n) takes a tree and “cuts off” all nodes further than n from the root."
prune :: Int -> Tree Position -> Tree Position
prune 0 (Node x ns) = Node x []
prune n (Node x ns) = Node x (map (prune (n-1)) ns)

-- "[a function] that would take a position and return its true value."
evaluate :: Position -> Int
evaluate = maximize . fmap static . prune 5 . gametree

--testEvaluate = evaluate emptyP
