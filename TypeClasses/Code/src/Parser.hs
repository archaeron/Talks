module Parser where

import Data.Char
import Control.Applicative (Applicative(..))
import Control.Monad (liftM, ap)

data Parser a =
	P { parse :: String -> Maybe (a, String) }

instance Functor Parser where
	fmap = liftM

instance Applicative Parser where
	pure a = P (\input -> Just (a, input))
	(<*>) = ap

instance Monad Parser where
	return = pure

	p >>= f =
		P $ \input ->
			case parse p input of
				Nothing ->
					Nothing
				Just (value, output) ->
					parse (f value) output


-- this parser always fails
failure :: Parser a
failure = P $ const Nothing

-- this parser always succeeds with the given value
success :: a -> Parser a
success a = P $ \input -> Just (a, input)

-- this parser reads the first character of the input string
-- and returns it
-- example: `parse item "hello" -- Just ('h',"ello")`
item :: Parser Char
item =
	P
		(\inp -> case inp of
			[] ->
				Nothing
			(x:xs) ->
				Just (x, xs)
		)

-- tries the first parser. if it fails it tries the second one
(+++) :: Parser a -> Parser a -> Parser a
p +++ q =
	P (\inp -> case parse p inp of
		Nothing	->
			parse q inp
		value ->
			value)

sat :: (Char -> Bool) -> Parser Char
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

sepBy1 :: Parser a -> Parser sep -> Parser [a]
sepBy1 p sep =
	do	a <- p
		as <- many $ do
			sep
			p
		return (a:as)

sepBy :: Parser a -> Parser sep -> Parser [a]
sepBy p sep	= (sepBy1 p sep) +++ return []
