import Data.Array (Array,listArray,(!))

type Vertex = Int
type Edge = (Vertex,Vertex)
type Graph = Array Vertex [Vertex]

text = "2\n5 4 0\n0 1\n0 2\n1 3\n2 4\n4 1 2\n2 3"


main = do
  txt <- pure text
  --txt <- readFile "in.txt"
  let (n,m,v0,edges) = parse txt
  print (dfs graph v0)
  print (bfs graph v0)
  putStrLn "goodbye"

parse :: String -> (Int,Int,Int,[Edge])
parse txt = (n,m,v0,edges) where
  lns = lines txt
  [n,m,v0] = map read (head lns)
  edges = (\[u,v] -> (read u, read v)) . words <$> tail lns

dfs :: Graph -> Vertex -> [Vertex]
dfs graph start = run [] [start] where
  run seen [] = seen
  run seen (node:stack)
    | node`elem`seen = run (node:seen) stack
    | otherwise      = run (node:seen) (neighbors++stack)
    where neighbors = graph ! node

bfs :: Graph -> Vertex -> [Vertex]
bfs graph start = run [] [start] where
  run seen [] = seen
  run seen (node:stack)
    | node`elem`seen = run (node:seen) stack
    | otherwise      = run (node:seen) (stack++neighbors)
    where neighbors = graph ! node
