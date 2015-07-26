module JsonParser where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Either
import Parser

type JResult = Either String

type JObject
    = Map.Map String JValue

type JArray = [JValue]

-- we will only use integers
data JValue
    = JObject JObject
    | JArray JArray
    | JString String
    | JNumber Int
    | JBool Bool
    | JNull
    deriving Show

class ToJSON a where
    toJSON :: a -> JValue

-- converts a (key, value)-pair to a string
showKeyValue :: (String, JValue) -> String
showKeyValue (k, v) = show k ++ ": " ++ valueToString v

-- converts a JObject to a comma separated list of (key, value)-pairs
jObjectToString :: JObject -> String
jObjectToString o =
    List.intercalate ", " (map showKeyValue $ Map.toAscList o)

-- converts a JArray to a comma separated list of values
jArrayToString :: JArray -> String
jArrayToString a = List.intercalate ", " $ map valueToString a

valueToString :: JValue -> String
valueToString (JObject o) = "{" ++ jObjectToString o ++ "}"
valueToString (JArray a) = "[" ++ jArrayToString a ++ "]"
valueToString (JString s) = show s
valueToString (JNumber n) = show n
valueToString (JBool True) = "true"
valueToString (JBool False) = "false"
valueToString JNull = "null"
 
encode :: (ToJSON a) => a -> String
encode = valueToString . toJSON







class FromJSON a where
    parseJSON :: JValue -> JResult a

parseJNull :: Parser JValue
parseJNull = fmap (const JNull) $ string "null"

parseJBool :: Parser JValue
parseJBool =
    fmap
        (\b ->
            case b of
            "true" -> JBool True
            _ -> JBool False
        )
        (string "true" +++ string "false")
