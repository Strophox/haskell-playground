import System.Environment (getProgName,getArgs)
import System.Exit        (exitFailure,exitSuccess)

main = do
  self <- getProgName
  args <- getArgs
  code <- case args of
      [arg] -> readFile arg
      _     -> putStrLn ("Usage: "<>self<>" [file]") >> exitFailure
  interact (run code) --putStr (run code "test124")

run :: String -> String -> String
run program input = go ([],program) ([],repeat 0) input where
  go :: (String,String) -> ([Int],[Int]) -> String -> String
  go (_,[]) _ _ = "" -- Program finished
  go (cl,c:cr) (ml,m:mr) is = case c of
    '<' | null ml -> error "left end of tape" -- (QOL)
    '<' -> go (c:cl,cr) (tail ml, head ml:m:mr) is
    '>' -> go (c:cl,cr) (m:ml, mr) is
    '+' -> go (c:cl,cr) (ml, (succ m`mod`256):mr) is
    '-' -> go (c:cl,cr) (ml, (pred m`mod`256):mr) is
    '.' -> toEnum m : go (c:cl,cr) (ml,m:mr) is
    ',' | null is -> "# input ended" -- (QOL)
    ',' -> go (c:cl,cr) (ml, fromEnum (head is):mr) (tail is)
    '[' | m==0 -> let (l,r) = match (c:cl,cr) 1
        in go (l,r) (ml,m:mr) is
    ']' | m/=0 -> let (r,l) = match (c:cr,cl) (-1)
        in go (l,r) (ml,m:mr) is
    _   -> go (c:cl,cr) (ml,m:mr) is

  match :: (String,String) -> Int -> (String,String)
  match (l,c:r) 0 = (l,c:r)
  match (_, []) n = error ("unmatched "<>(if n>0 then "'['" else "']'") ) -- (QOL)
  match (l,c:r) n = match (c:l,r) (case c of '[' -> n+1; ']' -> n-1; _ -> n)
