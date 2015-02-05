module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad (liftM)
import Control.Applicative (pure, (<*>))
import Data.Char

import TypeClasses
import Parser

instance Arbitrary a => Arbitrary (Option a) where
  arbitrary = frequency [(1, return None), (3, liftM Some arbitrary)]

  shrink (Some x) = None : [ Some x' | x' <- shrink x ]
  shrink _ = []

optionNeq :: (Eq a) => Option a -> Option a -> Bool
optionNeq (Some a) (Some b) = a /= b
optionNeq None None = False
optionNeq _ _ = True


main :: IO ()
main = hspec $ do
	optionTests
	parserTests

optionTests = describe "Option" $ do

	it "should be an instance of show" $ do
		let some1 = Some "value"
		let some2 = Some 5
		let none = (None :: Option Int)
		(show some1) `shouldBe` "Some \"value\""
		(show some2) `shouldBe` "Some 5"
		(show none) `shouldBe` "None"

	it "equal should be implemented correctly 1" $
		property $ \opt -> opt == (opt :: Option String)

	it "equal should be implemented correctly 2" $
		property $ \(opt1, opt2) ->
			opt1 /= opt2 ==>
				optionNeq (opt1 :: Option String) (opt2 :: Option String)

	it "follows the functor laws: fmap id = id" $
		property $ \opt -> fmap id opt == (opt :: Option Int)

	it "follows the functor laws: fmap (p . q) = (fmap p) . (fmap q)" $
		property $
			\opt ->
				let
					add5 n = n + 5
					mul7 n = n * 7
				in
					fmap (add5 . mul7) (opt :: Option Int) == ((fmap add5) . (fmap mul7) $ opt)

	it "follows the applicative laws: identity (pure id <*> v = v)" $
		property $
			\opt ->
				((pure id) <*> opt) == (opt :: Option String)

	it "follows the applicative laws: composition (pure (.) <*> u <*> v <*> w = u <*> (v <*> w))" $
		property $
			\opt ->
				let
					add5 n = n + 5
					someAdd = Some add5
					mul7 n = n * 7
					someMul = Some mul7
				in
					((pure (.)) <*> someAdd <*> someMul <*> (opt :: Option Int)) ==
						(someAdd <*> (someMul <*> opt))

	it "follows the applicative laws: homomorphism (pure f <*> pure x = pure (f x))" $
		property $
			\opt ->
				let
					add5 n = n + 5
				in
					(((pure add5) :: Option (Int -> Int)) <*> (pure (opt :: Int))) ==
						pure (add5 opt)

	it "follows the applicative laws: interchange (u <*> pure y = pure ($ y) <*> u)" $
		property $
			\opt ->
				let
					add5 n = n + 5
					someAdd = Some add5
				in
					(someAdd <*> (pure (opt :: Int))) ==
						(pure (\f -> f opt) <*> someAdd)

	it "follows the monad laws: return a >>= k = k a" $
		property $
			\opt ->
				let
					add5Some n = Some $ n + 5
				in
					((return (opt :: Int)) >>= add5Some) == add5Some opt

	it "follows the monad laws: m >>= return = m" $
		property $
			\opt ->
				((opt :: Option String) >>= return) == opt

	it "follows the monad laws: m >>= (\\x -> k x >>= h) = (m >>= k) >>= h" $
		property $
			\opt ->
				let
					add5Some n = Some $ n + 5
					mul7Some n = Some $ n + 5
				in
					((opt :: Option Int) >>= (\x -> add5Some x >>= mul7Some)) ==
						((opt >>= add5Some) >>= mul7Some)


parserTests = describe "Parser" $ do

	it "failure should fail" $
		property $
			\opt ->
				parse failure opt == (Nothing :: Maybe (Char, String))

	it "item should retrieve the first element" $ do
		parse item "Hello" `shouldBe` Just ('H', "ello")
		parse item "a" `shouldBe` Just ('a', "")
		parse item "" `shouldBe` Nothing

	it "(+++) works correctly" $ do
		parse (item +++ item) "Hello" `shouldBe` Just ('H', "ello")
		parse (item +++ failure) "Hello" `shouldBe` Just ('H', "ello")
		parse (failure +++ item) "Hello" `shouldBe` Just ('H', "ello")
		parse (failure +++ failure) "Hello" `shouldBe` (Nothing :: Maybe (Char, String))
