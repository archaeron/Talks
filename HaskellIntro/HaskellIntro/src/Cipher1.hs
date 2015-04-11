module Cipher1 where
    import Data.Char

    text :: String
    text = "keep calm and curry on"
    -- Tip: Check Data.Char docs for help


    main = putStrLn . encode 5 text