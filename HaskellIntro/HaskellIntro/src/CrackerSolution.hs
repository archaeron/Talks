module Main where

    import CipherSolution

    count :: Char -> String -> Int
    count x xs =  length [x' | x' <- xs, x == x']

    table :: [Float]
    table = [8.2, 1.5, 2.8, 4.3, 12.7, 2.2, 2.0,
             6.1, 7.0, 0.2, 0.8,  4.0, 2.4, 6.7,
             7.5, 1.9, 0.1, 6.0,  6.3, 9.1, 2.8,
             1.0, 2.4, 0.2, 2.0,  0.1]

    percent :: Int -> Int -> Float
    percent n m  =  (fromIntegral n / fromIntegral m) * 100


    freqs :: String -> [Float]
    freqs str = map (\x -> percent (length $ filter (\s -> s == x) str) $ length str) ['a'..'z']

    chisqr :: [Float] -> [Float] -> Float
    chisqr os es = sum [((o - e)^2) / e | (o,e) <- zip os es]

    rotate :: Int -> [a] -> [a]
    rotate n xs  =  drop n xs ++ take n xs

    positions :: Eq a => a -> [a] -> [Int]
    positions x xs =  [i | (x',i) <- zip xs [0..], x == x']


    crack :: String -> String
    crack xs = encode (-factor) xs
        where
            factor = head (positions (minimum chitab) chitab)
            chitab = [chisqr (rotate n table') table | n <- [0..25]]
            table' = freqs xs

    main = putStrLn (crack "pjju%hfqr%fsi%hzww~%ts")