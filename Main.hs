import GetDiffLines (editDistancePerLine, averageEditDistancePerLine, hammingDistancePerLine, averageHammingPerLine, getChanges, formatChanges, processDiff)
import FileHandler (writeResult, readFiles)
import System.IO
import System.FilePath
import System.Directory (doesFileExist)
import System.Environment ()
import Text.Printf (printf)



-- Não está sendo usada. printf resolve o problema!!
roundTo2 :: Double -> Double
roundTo2 average  = fromIntegral (round (average * 100)) / 100


main :: IO()
main = loop

missingInfo :: Int -> Int -> String -> String -> String
missingInfo fileMissing missingLines file1 file2
    | fileMissing == 1 = "Arquivo '" ++ file2 ++ "' está faltando " ++ show missingLines  ++ " linhas" 
    | fileMissing == 0 = "Arquivo '" ++ file1 ++ "' está faltando " ++ show missingLines ++ " linhas"
    | otherwise = "Os arquivos tem o mesmo número de linhas"








diff :: Char -> FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> IO ()
diff algoritmo startFile1 file1 startFile2 file2 output = do
    contentStartFile1 <- readFile startFile1
    contentCurrentFile1 <- readFile file1
    contentStartFile2 <- readFile startFile2
    contentCurrentFile2 <- readFile file2

    let startLines1 = lines contentStartFile1
    let startLines2 = lines contentStartFile2

    let currentLines1 = lines contentCurrentFile1
    let currentLines2 = lines contentCurrentFile2

    let (dist, averagePerLine)
            | algoritmo == 'h' = (hammingDistancePerLine currentLines1 currentLines2, averageHammingPerLine currentLines1 currentLines2)
            | algoritmo == 'e' = (editDistancePerLine currentLines1 currentLines2, averageEditDistancePerLine currentLines1 currentLines2)
            | otherwise        = error "Erro: Algoritmo invalido" 
    
    
    let linesInfoFile1 = processDiff file1  startLines1 currentLines1
    let linesInfoFile2 = processDiff file2  startLines2 currentLines2
    let changesFile1 = formatChanges(getChanges startLines1 currentLines1)
    let changesFile2 = formatChanges(getChanges startLines2 currentLines2)

    let result = unlines [ "Diff = " ++ show d ++ ", Media = " ++  printf "%.2f" avg | (d, avg) <- zip dist (map roundTo2 averagePerLine)] ++ "\n" 
                        ++ unlines linesInfoFile1 ++ changesFile1 ++   unlines linesInfoFile2  ++ changesFile2
    if null output
        then putStrLn result
        else do
            writeFile output result

    writeCachedNewFile startFile1 currentLines1
    writeCachedNewFile startFile2 currentLines2



writeCachedNewFile :: FilePath -> [String] -> IO()
writeCachedNewFile cachedPath newContent = writeFile cachedPath(unlines newContent)



writeCachedFileFirstTime :: FilePath -> FilePath -> IO()   
writeCachedFileFirstTime fileStart cachedPath = do
    fileExists <- doesFileExist cachedPath
    if not fileExists 
        then do
            content <- readFile fileStart
            let lines1 = lines content
            writeFile cachedPath (unlines lines1)
    else return()



loop :: IO()
loop = do
    -- Lê os arquivos e salva suas versões iniciais para poder comparar inserção e remoção de linhas
    putStr "\nDigite o caminho do primeiro arquivo: "
    hFlush stdout
    f1 <- getLine
    let cachedPathF1 = "cached_files/" ++ takeFileName f1
    writeCachedFileFirstTime f1 cachedPathF1 
    putStr "Digite o caminho do segundo arquivo: "
    hFlush stdout
    f2 <- getLine
    let cachedPathF2 = "cached_files/" ++ takeFileName f2
    writeCachedFileFirstTime f2 cachedPathF2 

    putStr "Digite o algoritmo que deseja utilizar: hamming(h) ou edicao(e): "
    hFlush stdout
    algoritmo <- getLine
    let alg = if null algoritmo then error "Nenhum algoritmo especificado" else head algoritmo
    putStr "Digite o caminho para o arquivo de output (deixe vazio para mostrar na tela): "
    hFlush stdout
    output <- getLine
    processaEntradas cachedPathF1 f1 cachedPathF2 f2 output alg 
    putStr "Deseja calcular novamente (s/n)? "
    hFlush stdout
    op <- getLine
    if op == "s"
        then loop
        else putStrLn "Encerrado."

processaEntradas :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath -> Char -> IO()
processaEntradas startFile1 file1 startFile2 file2 output algoritmo =  diff algoritmo startFile1 file1 startFile2 file2 output

