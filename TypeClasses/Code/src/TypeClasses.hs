module TypeClasses where


data Option a =
  Some a | None
  deriving (Show)

instance Functor Option where
  fmap f (Some a) = Some (f a)
  fmap f None = None
