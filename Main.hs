import GetDiffLines (hammingDistancePerLineRestricted,
                     hammingDistancePerLine, 
                     averageHammingPerLineRestrict,
                     averageHammingPerLine)
import FileHandler (writeResult, readFiles)
import System.IO
import System.Environment


roundTo2 :: Double -> Double
roundTo2 average  = fromIntegral (round (average * 100)) / 100





diff :: FilePath -> FilePath -> Bool -> Maybe FilePath -> IO ()

diff file1 file2 ignoreLines outputFile = do
    content1 <- readFile file1
    content2 <- readFile file2
    let lines1 = lines content1
        lines2 = lines content2
        dist =  if ignoreLines
                then hammingDistancePerLineRestricted lines1 lines2
                else hammingDistancePerLine lines1 lines2
        averagePerLine = if ignoreLines 
                         then averageHammingPerLineRestrict lines1 lines2
                         else averageHammingPerLine lines1 lines2
        finalResultLine = zip dist (map roundTo2 averagePerLine)
        output = unlines (map show finalResultLine) 

    case outputFile of
        Just file -> writeFile file output
        Nothing -> putStrLn output

            

main :: IO()
main = do
    args <- getArgs
    let realArgs = tail args
    let ignoreLines = elem "-i" args || elem "--ignore-lines" args
    case realArgs of
        [file1, file2, option, outputFile] -> do
            diff file1 file2 ignoreLines  (Just outputFile)
        [file1, file2, optionOrOutputfile] -> do
            if ignoreLines 
            then diff file1 file2 ignoreLines Nothing
            else diff file1 file2 False (Just optionOrOutputfile)
        [file1, file2] -> do
            diff file1 file2 False Nothing
        _ -> putStrLn "Usage : diff <file1_path> <file2_path> <-i|--ignore-lines> <outputFilePath>"
    -- case tail args of 
    --     [file1, file2, option, outputFile] -> do
    --         let ignoreLines = elem "-i" args || elem "--ignore-lines" args
    --         diff file1 file2 ignoreLines (Just outputFile)
    --     -- Caso 4 argumentos, é certeza de arq1 arq2 -i/--ignore-lines outputFile
    --     -- [file1, file2, option, outputFile] ->   do
    --     --     -- let ignoreLines = option == "-i" || option == "--ignore-lines"
    --     --     let ignoreLines = elem "-i" args || elem "--ignore-lines" args
    --     --     print ignoreLines
    --     --     diff file1 file2 ignoreLines (Just outputFile)
    --     -- -- Caso 3 argumentos, verificar se o 3 argumento é -i/--ignore-lines ou outputfile
    --     -- [file1, file2, outputFile] -> do
    --     --     let ignoreLines = elem "-i" args || elem "--ignore-lines" args
    --     --     diff file1 file2 ignoreLines (Just outputFile)

    --         -- if optionOrOutput == "-i" || optionOrOutput == "--ignore-lines"
    --         --     then do
    --         --         diff file1 file2 True Nothing
    --         --     else do
    --         --         diff file1 file2 False (Just optionOrOutput)

    --     -- Caso 2 argumentos, arq1 e arq2 (sem -i ou arq de saida)
    --     -- [file1, file2] -> do
    --     --     diff file1 file2 False Nothing
    --     _ -> putStrLn "Usage : diff <file1_path> <file2_path> <-i|--ignore-lines> <outputFilePath>"
    

