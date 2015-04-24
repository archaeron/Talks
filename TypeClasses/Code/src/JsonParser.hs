module JsonParser where

import qualified Data.Map.Strict as Map
import qualified Data.List as List
import Data.Either

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

instance Show JValue where
    show (JObject o) = "JObject " ++ show o
    show (JArray a) = "JArray " ++ show a
    show (JString s) = "JString " ++ show s
    show (JNumber s) = "JNumber " ++ show s
    show (JBool s) = "JBool " ++ show s
    show JNull = "JNull"


class ToJSON a where
    toJSON :: a -> JValue

showKeyValue (k, v) = show k ++ ": " ++ valueToString v

jObjectToString o =
    List.intercalate ", " (map showKeyValue $ Map.toAscList o)


valueToString :: JValue -> String
valueToString (JObject o) = "{" ++ jObjectToString o ++ "}"
valueToString (JArray a) = "[" ++ (List.intercalate ", " $ map valueToString a) ++ "]"
valueToString (JString s) = show s
valueToString (JNumber n) = show n
valueToString (JBool True) = "true"
valueToString (JBool False) = "false"
valueToString JNull = "null"
 
encode :: (ToJSON a) => a -> String
encode = valueToString . toJSON







class FromJSON a where
    parseJSON :: JValue -> JResult a
