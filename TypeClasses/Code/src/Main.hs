module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad (liftM)

import TypeClasses

instance (Eq a) => Eq (Option a) where
  (Some x) == (Some y) = x == y
  None == None = True
  _ == _ = False

instance Arbitrary a => Arbitrary (Option a) where
  arbitrary = frequency [(1, return None), (3, liftM Some arbitrary)]

  shrink (Some x) = None : [ Some x' | x' <- shrink x ]
  shrink _        = []

main :: IO ()
main = hspec $ do
  describe "Option" $ do
    it "follows the functor laws: fmap id = id" $
      property $ \opt -> fmap id opt == (opt :: Option Int)

    it "follows the functor laws: fmap (p . q) = (fmap p) . (fmap q)" $
      property $
        \opt ->
          fmap (add5 . mul7) (opt :: Option Int) == ((fmap add5) . (fmap mul7) $ opt)
        where
          add5 n = n + 5
          mul7 n = n * 7
