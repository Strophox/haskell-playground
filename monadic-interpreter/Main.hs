{- Tutorial "How to build a monadic interpreter in one day" by Dan Popa (Dept. Comp.Sci. Univ. Bacau, Romania) -}

import Frontend
import Backend

main = do
  putStrLn "hello, world"
  putStrLn ""

  putStrLn "s1_text = "
  print s1_text
  putStrLn ""

  putStrLn "apply com s1_text = "
  print (apply com s1_text)
  putStrLn ""

  putStrLn "s1_ast = "
  print s1_ast
  putStrLn ""

  putStrLn "interp s1_ast = "
  print (interp s1_ast)
  putStrLn ""

  putStrLn "run s1_text = "
  print (run s1_text)
  putStrLn ""

  putStrLn "run s1'_text = "
  print (run s1'_text)
  putStrLn ""

  putStrLn "run progS1_txt = "
  progS1_txt <- readFile "progS1.txt"
  print (run progS1_txt)
  putStrLn ""

  putStrLn "goodbye"

run = (\(_,_,x) -> x) . interp . fst . head . apply com

s1_text = "declare x = 150 in declare y = 200 in { while x > 0 do { x := x - 1 ; y := y - 1 } ; print y }"

s1'_text = "\
\declare x = 150 in\n\
\  declare y = 200 in\n\
\    {while x > 0 do { x:=x-1; y:=y-1 };\n\
\      print y\n\
\    }"

s1_ast =
  (Declare "x" (Constant 150)
    (Declare "y" (Constant 200)
      (Seq
        (While (Greater (Variable "x") (Constant 0))
          (Seq
            (Assign "x" (Minus (Variable "x") (Constant 1)))
            (Assign "y" (Minus (Variable "y") (Constant 1)))
          )
        )
        (Print (Variable "y"))
      )
    )
  )
