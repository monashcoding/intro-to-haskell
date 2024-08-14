module Calculator (calculator) where

import Control.Applicative ((<|>))
import Control.Monad (unless)
import Data.Maybe (listToMaybe)
import System.IO (hFlush, stdout)
import Text.ParserCombinators.ReadP
  ( ReadP
  , between
  , chainl1
  , chainr1
  , char
  , eof
  , readP_to_S
  , skipSpaces
  )
import Text.Read (readPrec, readPrec_to_P)

import Part3
  ( Expression (Add, Divide, Multiply, Negate, Number, Power, Subtract)
  , evaluateExpression
  )

calculator :: IO ()
calculator = do
  putStr "Enter an expression (type 'quit' to exit): "
  hFlush stdout
  input <- getLine
  unless (input == "quit") $ do
    putStrLn $ maybe "Invalid expression" (show . evaluateExpression) $ parse expr input
    calculator

{- This code uses parser combinators, which are very convenient for parsing things in Haskell.
  <|> means 'or'
  chainl1 repeatedly parses something followed by a separator from the left
  chainlr repeatedly parses something followed by a separator from the right
As you hopefully may be able to see, a parser for this calculator language is very concise and
arguably more readable than a hand-written imperative one with while/for loops might be.

If you're interested in learning more, just ask me (Lauren)!
-}

parse :: ReadP a -> String -> Maybe a
parse p = fmap fst . listToMaybe . readP_to_S (p <* eof)

tok :: ReadP a -> ReadP a
tok = (<* skipSpaces)

charTok :: Char -> ReadP Char
charTok = tok . char

double :: ReadP Double
double = readPrec_to_P readPrec 0

primary :: ReadP Expression
primary = Number <$> tok double <|> between (charTok '(') (charTok ')') expr

op :: Char -> a -> ReadP a
op c f = f <$ charTok c

neg :: ReadP Expression
neg = op '-' Negate <*> primary <|> primary

power :: ReadP Expression
power = chainr1 neg (op '^' Power) <|> neg

mul :: ReadP Expression
mul = chainl1 power (op '*' Multiply <|> op '/' Divide) <|> power

add :: ReadP Expression
add = chainl1 mul (op '+' Add <|> op '-' Subtract) <|> mul

{- |
>>> parse expr "1"
Just (Number 1.0)
>>> parse expr "1.2"
Just (Number 1.2)
>>> parse expr "-1"
Just (Negate (Number 1.0))
>>> parse expr "1 + 2  "
Just (Add (Number 1.0) (Number 2.0))
>>> parse expr "  (1+2) * -(3)"
Just (Multiply (Add (Number 1.0) (Number 2.0)) (Negate (Number 3.0)))
>>> parse expr "1 +2 + 3"
Just (Add (Add (Number 1.0) (Number 2.0)) (Number 3.0))
>>> parse expr "1^2^3"
Just (Power (Number 1.0) (Power (Number 2.0) (Number 3.0)))
>>> parse expr "1^2^3"
Just (Power (Number 1.0) (Power (Number 2.0) (Number 3.0)))
>>> parse expr "1 + 2 * 3 / -(4 ^ 5)"
Just (Add (Number 1.0) (Divide (Multiply (Number 2.0) (Number 3.0)) (Negate (Power (Number 4.0) (Number 5.0)))))
>>> parse expr "3 * -1"
Just (Multiply (Number 3.0) (Negate (Number 1.0)))
>>> parse expr "1."
Nothing
>>> parse expr ".2"
Nothing
>>> parse expr "3 *"
Nothing
>>> parse expr ""
Nothing
-}
expr :: ReadP Expression
expr = skipSpaces *> add
