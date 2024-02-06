module Frontend where

import Backend
import Control.Applicative (Alternative (empty,(<|>)))
import Control.Monad (MonadPlus (mzero,mplus))

{-Grammar

<rexp>   ::= <rexp> <relop> <expr> | <expr>
<expr>   ::= <expr> <addop> <term> | <term>
<term>   ::= <term> <mulop> <factor> |< factor>
<factor> ::= <var> | <digiti> | ( <expr> )
<var>    ::= <Identifier>
<digiti> ::= <digit> | <digit> <digiti>
<digit>  ::= 0 | 1 | ... | 9
<addop>  ::= + | -
<mulop>  ::= * | /
<relop>  ::= > | < | =

<com>     ::= <assign> | <seqv> | <cond> | <while> | <declare> | <printe>
<assign>  ::= <identif> ":=" <rexp>
<seqv>    ::= "{" <com> ";" <com> "}"
<cond>    ::= "if" <rexp> "then" <com> "else" <com>
<while>   ::= "while" <rexp> "do" <com>
<declare> ::= "declare" <identif> "=" <rexp> "in" <com>
<printe>  ::= "print" <rexp>

-}

-- Helpers.

isSpace, isAlpha, isDigit, isAlphaNum :: Char -> Bool
isSpace = (`elem`" \t\n\r")
isAlpha = (`elem`['A'..'Z']++['a'..'z'])
isDigit = (`elem`['0'..'9'])
isAlphaNum c = isAlpha c || isDigit c


-- Parser combinators. --

newtype Parser a = Parser { parse :: (String -> [(a,String)]) }

instance Functor Parser where
  fmap f p = Parser $ \str -> [ (f a,str1) | (a,str1)<-parse p str ]

instance Applicative Parser where
  pure x = Parser $ \str -> [(x,str)]
  p <*> q = Parser $ \str -> [ (f a,str2) | (f,str1)<-parse p str,
                                            (a,str2)<-parse q str1 ]

instance Monad Parser where
  return = pure
  p >>= f = Parser $ \str -> [ (b,str2) | (a,str1)<-parse p str,
                                          (b,str2)<-parse (f a) str1 ]

instance Alternative Parser where
  empty = Parser $ \str -> []
  p <|> q = Parser $ \str -> parse p str ++ parse q str

instance MonadPlus Parser where
  mzero = empty
  mplus = (<|>)


-- "The parser for a single character."
item :: Parser Char
item = Parser $ \str -> case str of "" -> [] ; (c:cs) -> [(c,cs)]

-- "Deterministic choice of the first possibility of parsing."
(+++) :: Parser a -> Parser a -> Parser a
p +++ q = Parser $ \str -> case parse (p`mplus`q) str of [] -> [] ; (x:xs) -> [x]

-- "Using a predicate we can create a parser which is able to respect a specific rule."
sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then
    return c
  else
    mzero
--sat p = item >>= \c -> if p c then return c else mzero

-- "A more general combinator may be defined."
infix 7 ?
(?) :: Parser a -> (a -> Bool) -> Parser a
p ? test = do
  a <- p
  if test a then
    return a
  else
    mzero
--p ? test = p >>= \a -> if test a then return a else mzero

-- "This function is able to create the parser for a specific Char."
char :: Char -> Parser Char
char c = sat (==c)

-- "This function is able to crate the parser for a speciic String."
string :: String -> Parser String
string "" = return ""
string (c:cs) = do char c
                   string cs
                   return (c:cs)
--string (c:cs) = char c >> string cs >> return (c:cs)

-- (no explanation given)
many :: Parser a -> Parser [a]
many p = many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p = do a <- p
             as <- many p
             return (a:as)
--many1 p = p >>= \a -> many p >>= \as -> return (a:as)

-- "A parser for spaces, blanks and tabs."
whitespace :: Parser String
whitespace = many (sat isSpace)

-- "Tokens followed by spaces (usual situation) can be parsed using 'token'."
token :: Parser a -> Parser a
token p = do a <- p
             whitespace
             return a
--token p = p >>= \a -> whitespace >> return a

-- "Symbol is used to parse a specific keyword (or a specific string) followed by spaces."
symbol :: String -> Parser String
symbol str = token (string str)

-- "Parser for identifiers."
ident :: Parser [Char]
ident = do l <- sat isAlpha
           lsc <- many (sat isAlphaNum)
           return (l:lsc)

-- "...and may be followed by spaces."
identif :: Parser [Char]
identif = token ident

-- "Parser for variables"
var :: Parser Exp
var = do v <- identif
         return (Variable v)

-- "Rules of the grammar like  a ::= a op b | b  [...]"
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) +++ return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = let
    rest x = (do f <- op
                 b <- p
                 rest (f x b)) +++ return x
  in do a <- p
        rest a

-- Combine parsers to incrementally parse greater parts of our grammar.

digit :: Parser Exp
digit = do x <- token (sat isDigit)
           return (Constant (fromEnum x - fromEnum '0'))

digiti :: Parser Exp
digiti = do l <- many1 digit
            return (foldl (\(Constant a) (Constant b) -> Constant (10*a+b)) (Constant 0) l)

-- "Parser for expressions [...]"

rexp :: Parser Exp
rexp = expr `chainl1` relop

expr :: Parser Exp
expr = term `chainl1` addop

term :: Parser Exp
term = factor `chainl1` mulop

-- "Parser for factor, where  factor ::= var | digiti | (expr)"
factor :: Parser Exp
factor = var +++ digiti +++ (symbol "(" >> rexp >>= \n -> symbol ")" >> return n)

-- "The parser for operators are using the data-constructors."
addop :: Parser (Exp -> Exp -> Exp)
addop = (symbol "-" >> return Minus) +++ (symbol "+" >> return Plus)

mulop :: Parser (Exp -> Exp -> Exp)
mulop = (symbol "*" >> return Times) +++ (symbol "/" >> return Divide)

relop :: Parser (Exp -> Exp -> Exp)
relop = (symbol ">" >> return Greater) +++ (symbol "=" >> return Equal) +++ (symbol "<" >> return Less) +++ (symbol "!=" >> return NotEq)

-- "Parsers for commands [...]"
{- Disclaimer: "This kind parsers did not consume the blanks from the begining of the text. A special function which is applied before parsing should deal with this kind of blanks or tabs." -}

printe :: Parser Com
printe = do
  symbol "print"
  x <- rexp
  return (Print x)

assign :: Parser Com
assign = do
  x <- identif
  symbol ":="
  e <- rexp
  return (Assign x e)

seqv :: Parser Com
seqv = do
  symbol "{"
  c <- com
  symbol ";"
  d <- com
  symbol "}"
  return (Seq c d)

cond :: Parser Com
cond = do
  symbol "if"
  e <- rexp
  symbol "then"
  c <- com
  symbol "else"
  d <- com
  return (Cond e c d)

-- "while ::= 'while' rexp 'do' com"
while :: Parser Com
while = do
  symbol "while"
  e <- rexp
  symbol "do"
  c <- com
  return (While e c)

-- "declare ::= 'declare' identif '=' rexp 'in' com"
declare :: Parser Com
declare = do
  symbol "declare"
  x <- identif
  symbol "="
  e <- rexp
  symbol "in"
  c <- com
  return (Declare x e c)

-- "com ::= assign | seqv | cond | while | declare | printe"
com :: Parser Com
com = assign +++ seqv +++ cond +++ while +++ declare +++ printe

-- "In order to use a parser and simultaneously discard spaces from the beginning of the text, 'apply' was defined as follows:"
apply :: Parser a -> String -> [(a,String)]
apply p = parse (whitespace >> p)
