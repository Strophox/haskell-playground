import System.Environment (getProgName,getArgs)
import System.Exit (exitFailure,exitSuccess)

main = do
  progName <- getProgName
  args <- getArgs
  code <- case args of
    [arg] -> readFile arg
    _     -> putStrLn ("Usage: "<>progName<>" [file]") >> exitFailure
  interact (run code) --putStr (run code "test124")

run :: String -> String -> String
run code input = go ([],code) ([],repeat 0) input where
  go :: (String,String) -> ([Int],[Int]) -> String -> String
  go (_,[]) _ _ = "# program finished.\n"
  go (cl,c:cr) (ml,m:mr) i = case c of
    '<' -> go (c:cl,cr) (tail ml, head ml:m:mr) i
    '>' -> go (c:cl,cr) (m:ml, mr) i
    '+' -> go (c:cl,cr) (ml, (succ m`mod`256):mr) i
    '-' -> go (c:cl,cr) (ml, (pred m`mod`256):mr) i
    '.' -> toEnum m : go (c:cl,cr) (ml,m:mr) i
    ',' | null i -> "# input ended.\n"
    ',' -> go (c:cl,cr) (ml, fromEnum (head i):mr) (tail i)
    '[' | m==0 -> let (l,r) = findMatch (c:cl,cr) 1
        in go (l,r) (ml,m:mr) i
    ']' | m/=0 -> let (r,l) = findMatch (c:cr,cl) (-1)
        in go (l,r) (ml,m:mr) i
    _   -> go (c:cl,cr) (ml,m:mr) i

  findMatch :: (String,String) -> Int -> (String,String)
  findMatch (l,c:r) n = if n == 0 then (l,c:r) else findMatch (c:l,r) (case c of '[' -> n+1 ; ']' -> n-1 ; _ -> n)
