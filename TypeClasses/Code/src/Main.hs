module Main where

import Test.Hspec
import Test.QuickCheck
import Control.Exception (evaluate)
import Control.Monad (liftM)
import Control.Applicative (pure, (<*>))
import Data.Char
import qualified Data.Map.Strict as Map

import TypeClasses
import Parser
import JsonParser

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
	jsonTests

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

	it "item works" $
		property $
			\opt ->
				if opt /= ""
				then
					fmap (\(x, xs) -> x:xs) (parse item opt) == return opt
				else
					(parse item opt) == Nothing

	it "item should retrieve the first element" $ do
		parse item "Hello" `shouldBe` Just ('H', "ello")
		parse item "a" `shouldBe` Just ('a', "")
		parse item "" `shouldBe` Nothing

	it "(+++) works correctly" $ do
		parse (item +++ item) "Hello" `shouldBe` Just ('H', "ello")
		parse (item +++ failure) "Hello" `shouldBe` Just ('H', "ello")
		parse (failure +++ item) "Hello" `shouldBe` Just ('H', "ello")
		parse (failure +++ failure) "Hello" `shouldBe` (Nothing :: Maybe (Char, String))

	it "sat works correctly" $ do
		parse digit "123" `shouldBe` Just ('1', "23")
		parse lower "Hello" `shouldBe` Nothing
		parse letter "1Hello" `shouldBe` Nothing
		parse (char 'H') "Hello" `shouldBe` Just ('H', "ello")
		parse (char 'e') "Hello" `shouldBe` Nothing

	it "string works correctly" $ do
		parse (string "12") "123" `shouldBe` Just ("12", "3")
		parse (string "ello") "Hello" `shouldBe` Nothing
		parse (string "H") "Hello" `shouldBe` Just ("H", "ello")
		parse (string "") "Hello" `shouldBe` Just ("", "Hello")
		parse (string "He") "" `shouldBe` Nothing
		parse (string "hello") "hel" `shouldBe` Nothing

	it "many and many1 work correctly" $ do
		parse (many (string "12")) "12123" `shouldBe` Just (["12", "12"], "3")
		parse (many digit) "123Hello" `shouldBe` Just (['1', '2', '3'], "Hello")
		parse (many digit) "Hello" `shouldBe` Just ([], "Hello")
		parse (many1 (string "12")) "12123" `shouldBe` Just (["12", "12"], "3")
		parse (many1 digit) "123Hello" `shouldBe` Just (['1', '2', '3'], "Hello")
		parse (many1 digit) "Hello" `shouldBe` Nothing

	it "sepBy works correctly" $ do
		parse (sepBy1 digit (char ',')) "1,2,3" `shouldBe` Just ("123", "")
		parse (sepBy1 digit (char '-')) "1,2,3" `shouldBe` Just ("1", ",2,3")
		parse (sepBy1 letter (char ',')) "1,2,3" `shouldBe` Nothing

		parse (sepBy digit (char ',')) "1,2,3" `shouldBe` Just ("123", "")
		parse (sepBy digit (char '-')) "1,2,3" `shouldBe` Just ("1", ",2,3")
		parse (sepBy letter (char ',')) "1,2,3" `shouldBe` Just ([], "1,2,3")

jsonTests = describe "jSON" $ do
	it "converts correctly to strings" $ do
		let jbool = JBool True
		let jnumber = JNumber 56
		let jstring = JString "Hello World"
		let jarray = JArray [jbool, jnumber, jstring]
		let jobject = JObject $ Map.fromList [("bool", jbool), ("number", jnumber), ("string", jstring)]

		valueToString jbool `shouldBe` "true"
		valueToString jnumber `shouldBe` "56"
		valueToString jstring `shouldBe` "\"Hello World\""
		valueToString jarray `shouldBe` "[true, 56, \"Hello World\"]"
		valueToString jobject `shouldBe` "{\"bool\": true, \"number\": 56, \"string\": \"Hello World\"}"
