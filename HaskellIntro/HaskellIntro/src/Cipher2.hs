module Cipher2 where
    import Data.Char

    text :: String
    text = "keep calm and curry on"
    -- Tip: Check Data.Char docs for help:
    -- functions to look for: ord and chr

    letterToInt :: Char -> Int
    intToLetter :: Int -> Char
    shift :: Int -> Char -> Char
    encode :: Int -> String -> String


    main = putStrLn $ encode 5 text
