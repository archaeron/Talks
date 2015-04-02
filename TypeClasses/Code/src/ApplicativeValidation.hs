module ApplicativeValidation where

import Control.Applicative
import Data.Either

data Address = Address { street :: String, city :: String, canton :: String } deriving (Show)

address1 :: Maybe String -> Maybe String -> Maybe String -> Maybe Address
address1 street city canton =
	liftA3 Address street city canton

address2 :: Maybe String -> Maybe String -> Maybe String -> Maybe Address
address2 street city canton =
	Address <$> street <*> city <*> canton

(<?>) Nothing err = Left err
(<?>) (Just a) _ = Right a

address3 :: Maybe String -> Maybe String -> Maybe String -> Either String Address
address3 street city canton =
	Address <$> (street <?> "street was missing")
		<*> (city <?> "city was missing")
		<*> (canton <?> "canton was missing")


type Errors = [String]

nonEmpty :: String -> String -> Either Errors Unit
nonEmpty field "" = Left ["Field '" ++ field ++ "' cannot be empty"]
nonEmpty _ _ = pure unit

lengthIs :: String -> Number -> String -> V Errors Unit
lengthIs field len value | S.length value /= len =
	invalid ["Field '" ++ field ++ "must have length " ++ show len]
lengthIs _ _ _ =
	pure unit

validateAddress :: Address -> V Errors Address
validateAddress (Address o) =
	address <$> (nonEmpty "Street" o.street *> pure o.street)
		<*> (nonEmpty "City" o.city *> pure o.city)
		<*> (lengthIs "State" 2 o.state *> pure o.state)

-- validateAddress $ address "" "" ""
-- validateAddress $ address "" "" "CA"
