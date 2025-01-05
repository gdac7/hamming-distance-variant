module FileHandler where

import System.IO

-- Readfiles receive two String (FilePath is just an alias for String), performs some IO operation (Indacated by IO ) and returns an obj
-- a, where in this case a ins ([String], [String])
readFiles :: FilePath -> FilePath -> IO ([String], [String])    
readFiles file1 file2 = do
    content1 <- readFile file1
    content2 <- readFile file2
    return (lines content1, lines content2)

writeResult :: [String] -> FilePath -> IO()
writeResult results outputFile = writeFile outputFile(unlines results)

content = readFiles "teste/a.txt" "teste/b.txt"

{-
>>= bind operator
>>= exige uma função como segundo argumento
Le o conteudo de file1 e passa para content1 
Lê o conteudo de fil2 e passa para content2 
usa esses valores criados por \ no return
readFiles file1 file2 = 
    readFile file1 >>= \content1 ->
    readFile file2 >>= \content2 ->
    return (lines content1, lines content2)
-}