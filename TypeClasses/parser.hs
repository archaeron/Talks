module Parsing where

import Data.Char
import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad	   (liftM, ap)

newtype Parser a	=  Parser (String -> [(a, String)])

instance Functor Parser where
	fmap = liftM

instance Applicative Parser where
	pure = return
	(<*>) = ap


instance Monad Parser where
	return a = Parser (\input -> [(a, input)])

	p >>= f =
		Parser (\input ->
			case parse p input of
				[]					-> []
				[(value, output)]	-> parse (f value) output)


parse						:: Parser a -> String -> [(a, String)]
parse (Parser p)			=  p


failure						:: Parser a
failure						=  Parser $ const []

item						:: Parser Char
item						=  Parser (\inp -> case inp of
									[]	 -> []
									(x:xs) -> [(x,xs)])


(+++)						:: Parser a -> Parser a -> Parser a
p +++ q						=  Parser (\inp -> case parse p inp of
									[]	-> parse q inp
									xs	-> xs)

-- Derived primitives

sat							:: (Char -> Bool) -> Parser Char
sat p						= do
								x <- item
								if p x then return x else failure

digit						:: Parser Char
digit						=  sat isDigit

lower						:: Parser Char
lower						=  sat isLower

upper						:: Parser Char
upper						=  sat isUpper

letter						:: Parser Char
letter						=  sat isAlpha

alphanum					:: Parser Char
alphanum					=  sat isAlphaNum

char						:: Char -> Parser Char
char x						=  sat (== x)

string						:: String -> Parser String
string []					=  return []
string (x:xs)				=  do
								char x
								string xs
								return (x:xs)

many						 :: Parser a -> Parser [a]
many p						=  many1 p +++ return []

many1						:: Parser a -> Parser [a]
many1 p						= do
								v  <- p
								vs <- Parsing.many p
								return (v:vs)

sepBy						:: Parser a -> Parser sep -> Parser [a]
sepBy p sep					=
								do
									element <- p
									return []

ident						:: Parser String
ident						=  do
									x <- lower
									xs <- Parsing.many alphanum
									return (x:xs)

nat							:: Parser Int
nat							=
								do
									xs <- many1 digit
									return (read xs)

int							:: Parser Int
int							=
								(do
									char '-'
									n <- nat
									return (-n))
									+++ nat


space						:: Parser ()
space						=
								do
									Parsing.many (sat isSpace)
									return ()

comment						:: Parser ()
comment						=
								do
									string "---"
									Parsing.many (sat (/= '\n'))
									return ()
