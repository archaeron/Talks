module Cracker2 where

    import CipherSolution

    count :: Char -> String -> Int

    table :: [Float]
    table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0,
             6.1, 7.0, 0.2, 0.8,  4.0, 2.4, 6.7,
             7.5, 1.9, 0.1, 6.0,  6.3, 9.1, 2.8,
             1.0, 2.4, 0.2, 2.0,  0.1]

    percent :: Int -> Int -> Float

    freqs :: String -> [Float]

    chisqr :: [Float] -> [Float] -> Float

    rotate :: Int -> [a] -> [a]

    positions :: Eq a => a -> [a] -> [Int]

    crack :: String -> String

    main = putStrLn (crack "pjju%hfqr%fsi%hzww~%ts")