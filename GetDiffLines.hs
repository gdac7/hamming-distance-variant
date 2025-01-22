module GetDiffLines where 
import Data.List ((\\))
import Data.Array


roundTo2 :: Double -> Double
roundTo2 average  = fromIntegral (round (average * 100)) / 100    



-- Não desenvolvi esse algoritmo (quem dera). Peguei a base em https://wiki.haskell.org/Edit_distance. 
-- Não consegui fazer o meu hamming distance dar como resultado 1 em "ahello" vs "hello", etc...
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



getChanges :: [String] -> [String] -> [(String, String)]
getChanges cachedFile currentFile = [(x, y) | (x, y) <- zip cachedFile currentFile, x /= y]

-- Apenas para formatar o resultado de getChanges
formatChanges :: [(String, String)] -> String
formatChanges changes = unlines [ "(-" ++ x ++ ", +" ++ y ++ ")" | (x, y) <- changes ]




diffLines :: [String] -> [String] -> ([String], [String])
diffLines a b = 
    let removed = a \\ b 
        inserted = b \\ a 
    in (removed, inserted)


constructOutput :: String -> [String] -> String -> [String]
constructOutput file linesDiff prefix = 
    let header = file
        body = map (\line -> prefix ++ " " ++ line) linesDiff
    in if null linesDiff then [] else header : body


processDiff :: FilePath -> [String] -> [String] -> [String]
processDiff fileName a b =
    let (removed, inserted) = diffLines a b
        removedOutput = constructOutput fileName removed "--"
        insertedOutput = constructOutput fileName inserted "++"
    in removedOutput ++ insertedOutput




