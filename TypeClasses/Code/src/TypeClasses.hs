module TypeClasses where

import Control.Applicative (Applicative, pure, (<*>))

data Option a =
	Some a | None

instance (Show a) => Show (Option a) where
	show o = undefined

instance (Eq a) => Eq (Option a) where
	o == p	= undefined

instance Functor Option where
	fmap f o = undefined

instance Applicative Option where
	pure a = undefined
	o <*> p	= undefined

instance Monad Option where
	return a = undefined
	o >>= f = undefined
