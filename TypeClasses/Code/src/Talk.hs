module MonadIntroduction where

import Control.Applicative (liftA2)
import Control.Monad (liftM, ap)


-- Introduction

describeValue :: Show a => a -> String
describeValue a = "The values is: " ++ show a

-- Functor

data List a = Cons a (List a) | Nil deriving Show

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

list1 = Nil
list2 = Cons 4 Nil
list3 = Cons 4 $ Cons 10 Nil

-- Applicative Validation

data Address = Address
    { street :: String
    , city :: String
    } deriving Show

address1 = Address <$> (Just "Technikumstrasse 9") <*> (Just "Winterthur")
address2 = Address <$> (Just "Technikumstrasse 9") <*> Nothing
address3 = liftA2 Address (Just "Technikumstrasse 9") (Just "Winterthur")
address4 = liftA2 Address (Just "Technikumstrasse 9") Nothing

-- A better way for validation

(<?>) :: Maybe a -> String -> Either String a
(<?>) Nothing err = Left err
(<?>) (Just a) _ = Right a

addressEither street city =
    Address
        <$> (street <?> "the street is missing")
        <*> (city <?> "the city missing")















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

-- Exception 2

instance Functor MException where
    fmap = liftM

instance Applicative MException where
    pure = return
    (<*>) = ap

instance Monad MException where
    return = Return
    (Raise e) >>= _ = Raise e
    (Return a) >>= f = f a

-- Monad with the bind (>>=) function
evalException2 :: Term -> MException Int
evalException2 (Con a) = return a
evalException2 (Div t u) =
    (evalException2 t) >>=
        (\a -> evalException2 u >>=
            (\b ->
                 if b == 0
                    then Raise "divide by zero"
                    else return (a `div` b)))

-- Monad with do-notation
evalException3 :: Term -> MException Int
evalException3 (Con a) = return a
evalException3 (Div t u) =
    do
        a <- evalException3 t
        b <- evalException3 u
        if b == 0
        then Raise "divide by zero"
        else Return $ a `div` b

-- using `Either` instead of our own Monad
evalExceptionEither :: Term -> Either String Int
evalExceptionEither (Con a) = return a
evalExceptionEither (Div t u) =
    do
        a <- evalExceptionEither t
        b <- evalExceptionEither u
        if b == 0
        then Left "divide by zero"
        else Right $ a `div` b
