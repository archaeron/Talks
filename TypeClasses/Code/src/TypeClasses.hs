module TypeClasses where

data Option a =
  Some a | None
  deriving (Show)

instance (Eq a) => Eq (Option a) where
  (Some x) == (Some y) = x == y
  None == None = True
  _ == _ = False

instance Functor Option where
  fmap f (Some a) = Some (f a)
  fmap f None = None
