module Parser where

import Data.Char
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

newtype Parser a =
	Parser (String -> Maybe (a, String))

instance Functor Parser where
	fmap = liftM

instance Applicative Parser where
	pure a = Parser (\input -> Just (a, input))
	(<*>) = ap

instance Monad Parser where
	return = pure

	p >>= f =
		Parser $ \input ->
			case parse p input of
				Nothing ->
					Nothing
				Just (value, output) ->
					parse (f value) output


parse :: Parser a -> String -> Maybe (a, String)
parse (Parser p) = p

-- this parser always fails
failure :: Parser a
failure	= Parser $ const Nothing

item :: Parser Char
item =
	Parser
		(\inp -> case inp of
			[] ->
				Nothing
			(x:xs) ->
				Just (x, xs)
		)

(+++) :: Parser a -> Parser a -> Parser a
p +++ q	=
	Parser (\inp -> case parse p inp of
		Nothing	->
			parse q inp
		value ->
			value)

sat	:: (Char -> Bool) -> Parser Char
sat p =
	do	x <- item
		if p x then return x else failure

digit :: Parser Char
digit =  sat isDigit

lower :: Parser Char
lower =  sat isLower

upper :: Parser Char
upper =  sat isUpper

letter :: Parser Char
letter =  sat isAlpha

alphanum :: Parser Char
alphanum =  sat isAlphaNum

char :: Char -> Parser Char
char x = sat (== x)

string :: String -> Parser String
string [] =  return []
string (x:xs) =
	do	char x
		string xs
		return (x:xs)

many :: Parser a -> Parser [a]
many p =  many1 p +++ return []

many1 :: Parser a -> Parser [a]
many1 p =
	do	v  <- p
		vs <- many p
		return (v:vs)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep	=
	do	element <- p
		return []

ident :: Parser String
ident =
	do	x <- lower
		xs <- many alphanum
		return (x:xs)

nat	:: Parser Int
nat	=
	do	xs <- many1 digit
		return (read xs)

int	:: Parser Int
int	=
	(do	char '-'
		n <- nat
		return (-n))
	+++ nat

space :: Parser ()
space =
	do	many (sat isSpace)
		return ()

comment	:: Parser ()
comment	=
	do	string "---"
		many (sat (/= '\n'))
		return ()
