module GetDiffLines where 

import Data.Array

roundTo2 :: Double -> Double
roundTo2 average  = fromIntegral (round (average * 100)) / 100    

hammingRecursive :: String -> String -> Int
hammingRecursive [] [] = 0
hammingRecursive [] b = length b
hammingRecursive a [] = length a
hammingRecursive (x:xs) (y:ys)
    | x == y    = hammingRecursive xs ys
    | otherwise = 1 + hammingRecursive xs ys


-- List comprehension in haskell -> [output | pattern <- list, condition1, condition2, ...]
hamming :: String -> String -> Int
hamming a b = 
    let diffLength = abs (length a - length b)
        hamming = length [() | (x, y) <- zip a b, x /= y]-- length (filter (uncurry (/=)) (zip a b)) faz a mesma coisa! 
    in hamming + diffLength


hammingDistancePerLine :: [String] -> [String] -> [Int]
hammingDistancePerLine a b = zipWith hamming a b

averageHammingPerLine :: [String] -> [String] -> [Double]
--     a b 
averageHammingPerLine =
    zipWith (\x y -> 
        let diff = hamming x y
            total =  length x + length y
        in fromIntegral diff / fromIntegral total)




--Edit Distance
-- Esse código foi baseado no algoritmo https://wiki.haskell.org/Edit_distance#:~:text=The%20edit%20distance%20is%20the,using%20a%20dynamic%20programming%20algorithm.
editDistance :: String -> String -> Int
editDistance xs ys = table ! (m, n)
  where
    (m, n) = (length xs, length ys)
    x = array (1, m) (zip [1..] xs)  -- Converte a string xs em um array indexado
    y = array (1, n) (zip [1..] ys)  -- Converte a string ys em um array indexado

    table :: Array (Int, Int) Int
    table = array bnds [((i, j), dist i j) | (i, j) <- range bnds]
    bnds = ((0, 0), (m, n))  -- Define os limites do array

    dist 0 j = j  -- Caso base: distância de uma string vazia para outra
    dist i 0 = i  -- Caso base: distância para uma string vazia
    dist i j
      | x ! i == y ! j = table ! (i - 1, j - 1)  -- Sem custo se os caracteres forem iguais
      | otherwise = 1 + minimum
          [ table ! (i - 1, j),    -- Remoção
            table ! (i, j - 1),    -- Inserção
            table ! (i - 1, j - 1) -- Substituição
          ]


editDistancePerLine :: [String] -> [String] -> [Int]
editDistancePerLine a b = zipWith editDistance a b


averageEditDistancePerLine :: [String] -> [String] -> [Double]
averageEditDistancePerLine = 
    zipWith(\x y ->
        let editDist = editDistance x y
            total = length x + length y
            in fromIntegral editDist / fromIntegral total) -- Pq não fromInteger?

-- averageHammingPerLineResctrict a b =
--     zipWith (\x y -> 
--         let diff = hamming x y
--             total = max (length x) (length y)
--         in fromIntegral diff / fromIntegral total)


-- hammingDistancePerLine :: [String] -> [String] -> [Int]
-- hammingDistancePerLine a b = 
--     let maxLen = max (length a) (length b)
--         modA = take maxLen (a ++ repeat "")
--         modB = take maxLen (b ++ repeat "")
--     in zipWith hamming modA modB -- Mesmo funcionamento que map (uncurry hamming) (zip a b)
                                -- Ou até mesmo map (\(x, y) -> hamming x y) (zip a b)


    
-- averageHammingPerLine :: [String] -> [String] -> [Double]
-- averageHammingPerLine a b = 
--     let maxLen = max (length a) (length b)
--         modA = take maxLen (a ++ repeat "")
--         modB = take maxLen (b ++ repeat "")
--     in zipWith (\x y -> let diff = hamming x y
--                             total = max (length x) (length y) 
--                         in fromIntegral diff / fromIntegral total)
--                 modA modB


    


    
                


 





    



