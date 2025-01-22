module GetDiffLines where 

import Data.Array


roundTo2 :: Double -> Double
roundTo2 average  = fromIntegral (round (average * 100)) / 100    


editDistance :: String -> String -> Int
editDistance xs ys = table ! (m, n)
  where
    (m, n) = (length xs, length ys)
    x = array (1, m) (zip [1..] xs)  
    y = array (1, n) (zip [1..] ys)  

    table :: Array (Int, Int) Int
    table = array bnds [((i, j), dist i j) | (i, j) <- range bnds]
    bnds = ((0, 0), (m, n))  

    dist 0 j = j  
    dist i 0 = i  
    dist i j
      | x ! i == y ! j = table ! (i - 1, j - 1)  
      | otherwise = 1 + minimum
          [ table ! (i - 1, j),    
            table ! (i, j - 1),    
            table ! (i - 1, j - 1) 
          ]


editDistancePerLine :: [String] -> [String] -> [Int]
editDistancePerLine a b = zipWith editDistance a b


averageEditDistancePerLine :: [String] -> [String] -> [Double]
averageEditDistancePerLine = 
    zipWith(\x y ->
        let editDist = editDistance x y
            total = length x + length y
            in fromIntegral editDist / fromIntegral total) -- Pq não fromInteger?





-- List comprehension in haskell -> [output | pattern <- list, condition1, condition2, ...]
hamming :: String -> String -> Int
hamming a b = 
    let diffLength = abs (length a - length b)
        hamming = length [() | (x, y) <- zip a b, x /= y]-- length (filter (uncurry (/=)) (zip a b)) faz a mesma coisa! 
    in hamming + diffLength


hammingDistancePerLine :: [String] -> [String] -> [Int]
hammingDistancePerLine a b = zipWith hamming a b

averageHammingPerLine :: [String] -> [String] -> [Double]
averageHammingPerLine =
    zipWith (\x y -> 
        let diff = hamming x y
            total =  length x + length y
        in fromIntegral diff / fromIntegral total) 





getMissingLines :: [String] -> [String] ->(Int, Int)
getMissingLines a b
        | lengthA > lengthB = (lengthA - lengthB, 1)
        | lengthA < lengthB = (lengthB - lengthA, 0)
        | otherwise = (0, -1)
        where
            lengthA = length a
            lengthB = length b








-- hammingRecursive :: String -> String -> Int
-- hammingRecursive [] [] = 0
-- hammingRecursive [] b = length b
-- hammingRecursive a [] = length a
-- hammingRecursive (x:xs) (y:ys)
--     | x == y    = hammingRecursive xs ys
--     | otherwise = 1 + hammingRecursive xs ys



