module TypeClasses where

import Control.Applicative

data Option a =
    Some a | None
    deriving (Show)

instance (Show a) => Show (Option a) where
    show (Some a) = "Some " ++ show a
    show None = "None"

instance (Eq a) => Eq (Option a) where
    (Some x) == (Some y) = x == y
    None == None = True
    _ == _ = False

instance Functor Option where
    fmap f (Some a) = Some (f a)
    fmap f None = None

instance Applicative Option where
    pure a = Some a
    (Some f) <*> (Some a) = Some $ f a
    _ <*> _ = None