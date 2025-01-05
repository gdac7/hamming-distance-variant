module GetDiffLines where 

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


hammingDistancePerLineRestricted :: [String] -> [String] -> [Int]
hammingDistancePerLineRestricted a b = zipWith hamming a b

hammingDistancePerLine :: [String] -> [String] -> [Int]
hammingDistancePerLine a b = 
    let maxLen = max (length a) (length b)
        modA = take maxLen (a ++ repeat "")
        modB = take maxLen (b ++ repeat "")
    in zipWith hamming modA modB -- Mesmo funcionamento que map (uncurry hamming) (zip a b)
                                -- Ou até mesmo map (\(x, y) -> hamming x y) (zip a b)



averageHammingPerLineRestrict :: [String] -> [String] -> [Double]
-- averageHammingPerLineResctrict a b =
--     zipWith (\x y -> 
--         let diff = hamming x y
--             total = max (length x) (length y)
--         in fromIntegral diff / fromIntegral total)
--     a b 
averageHammingPerLineRestrict =
    zipWith (\x y -> 
        let diff = hamming x y
            total = max (length x) (length y)
        in fromIntegral diff / fromIntegral total)
    
averageHammingPerLine :: [String] -> [String] -> [Double]
averageHammingPerLine a b = 
    let maxLen = max (length a) (length b)
        modA = take maxLen (a ++ repeat "")
        modB = take maxLen (b ++ repeat "")
    in zipWith (\x y -> let diff = hamming x y
                            total = max (length x) (length y) 
                        in fromIntegral diff / fromIntegral total)
                modA modB


    


    
                


 





    



