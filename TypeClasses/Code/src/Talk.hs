module MonadIntroduction where

import Control.Monad (liftM, ap)

data Term = Con Int | Div Term Term
	deriving Show

answer :: Term
answer = Div (Div (Con 1972) (Con 2)) (Con 23)

errorTerm :: Term
errorTerm = Div (Con 42) (Con 0)

-- Simple

eval :: Term -> Int
eval (Con a) =  a
eval (Div t u) = eval t `div` eval u

-- Counter

type StateCounter = Int
type MCounter a = StateCounter -> (a, StateCounter)

evalCounter :: Term -> MCounter Int
evalCounter (Con a) x = (a, x)
evalCounter (Div t u) x =
	let (a, y) = evalCounter t x in
	let (b, z) = evalCounter u y in
	(a `div` b, z + 1)

-- Exceptions

type Exception = String
data MException a = Raise Exception | Return a

instance Show a => Show (MException a) where
	show (Raise e) = e
	show (Return a) = show a

evalException :: Term -> MException Int
evalException (Con a) = Return a
evalException (Div t u) =
	case evalException t of
		Raise e -> Raise e
		Return a ->
			case evalException u of
				Raise e -> Raise e
				Return b ->
					if b == 0
						then Raise "divide by zero"
						else Return (a `div` b)


-- Counter 2

-- Exception 2

instance Functor MException where
	fmap = liftM

instance Applicative MException where
	pure = return
	(<*>) = ap

instance Monad MException where
	return = Return
	a >>= k =
		case a of
			Raise e -> Raise e
			Return a -> k a

evalException2 :: Term -> MException Int
evalException2 (Con a) = return a
evalException2 (Div t u) =
	(evalException2 t) >>=
		(\a -> evalException2 u >>=
			(\b ->
			 	if b == 0
					then Raise "divide by zero"
					else return (a `div` b)))
