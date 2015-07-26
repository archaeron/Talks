
module Main where

    import Data.Char

    text :: String
    text = "keep calm and curry on"
    -- Tip: Check Data.Char docs for help:
    -- functions to look for: ord und chr

    letterToInt :: Char -> Int
    letterToInt x = ord x - ord 'a'

    intToLetter :: Int -> Char
    intToLetter n = chr (ord 'a' + n)

    shift :: Int -> Char -> Char
    shift n x = intToLetter . (+) n $ letterToInt x

    encode :: Int -> String -> String
    encode n s = map (shift n) s


    main =
        let
            encoded = encode 5 text
            decoded = encode (-5) encoded
        in
            putStrLn $ decoded ++ " to: " ++ encoded