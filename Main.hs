import GetDiffLines (editDistancePerLine, averageEditDistancePerLine, hammingDistancePerLine, averageHammingPerLine)
import FileHandler (writeResult, readFiles)
import System.IO
import System.Environment
import Text.Printf (printf)


-- Não está sendo usada. printf resolve o problema!!
roundTo2 :: Double -> Double
roundTo2 average  = fromIntegral (round (average * 100)) / 100


main :: IO()
main = loop




-- diff :: FilePath -> FilePath -> Bool -> Maybe FilePath -> IO ()

diff :: Char -> FilePath -> FilePath -> FilePath -> IO ()
diff algoritmo file1 file2 output = do
    content1 <- readFile file1
    content2 <- readFile file2
    let lines1 = lines content1
    let lines2 = lines content2
    let (dist, averagePerLine)
            | algoritmo == 'h' = (hammingDistancePerLine lines1 lines2, averageHammingPerLine lines1 lines2)
            | algoritmo == 'e' = (editDistancePerLine lines1 lines2, averageEditDistancePerLine lines1 lines2)
            | otherwise        = error "Erro: Algoritmo invalido" 
    
    --let finalResultLine = zip dist (map roundTo2 averagePerLine)
    -- Usar show na linha abaixo causa exibição de números em notação científica
    let result = unlines [ "Diff = " ++ show d ++ ", Media = " ++  printf "%.2f" avg | (d, avg) <- zip dist (map roundTo2 averagePerLine) ]

    if null output
        then putStrLn result
        else do
            writeFile output result


-- diff file1 file2 ignoreLines outputFile = do
--     content1 <- readFile file1
--     content2 <- readFile file2
--     let lines1 = lines content1
--         lines2 = lines content2
--         dist =  if ignoreLines
--                 then hammingDistancePerLineRestricted lines1 lines2
--                 else hammingDistancePerLine lines1 lines2
--         averagePerLine = if ignoreLines 
--                          then averageHammingPerLineRestrict lines1 lines2
--                          else averageHammingPerLine lines1 lines2
--         finalResultLine = zip dist (map roundTo2 averagePerLine)
--         output = unlines (map show finalResultLine) 

--     case outputFile of
--         Just file -> writeFile file output
--         Nothing -> putStrLn output

            

loop :: IO()
loop = do
    putStr "\nDigite o caminho do primeiro arquivo: "
    hFlush stdout
    f1 <- getLine
    putStr "Digite o caminho do segundo arquivo: "
    hFlush stdout
    f2 <- getLine
    putStr "Digite o algoritmo que deseja utilizar: hamming(h) ou edicao(e): "
    hFlush stdout
    algoritmo <- getLine
    {--A linha abaixo transforma 'algoritmo' para char. Bom para poupar memoria
        Poderia também ser feito com: 
        alg <- case algoritmo of
            [c] -> return c 
            [] -> error "Nenhum algoritmo especificado"
            _ -> error "Algoritmo invalido! Digite apenas 'h' ou 'e'"
    --}
    let alg = if null algoritmo then error "Nenhum algoritmo especificado" else head algoritmo
    putStr "Digite o caminho para o arquivo de output (deixe vazio para mostrar na tela): "
    hFlush stdout
    output <- getLine
    processaEntradas f1 f2 alg output
    putStr "Deseja calcular novamente? (s/n)"
    hFlush stdout
    op <- getLine
    if op == "s"
        then loop
        else putStrLn "Encerrado."
    -- args <- getArgs
    -- let realArgs = tail args
    -- let ignoreLines = elem "-i" args || elem "--ignore-lines" args
    -- case realArgs of
    --     [file1, file2, option, outputFile] -> do
    --         diff file1 file2 ignoreLines  (Just outputFile)
    --     [file1, file2, optionOrOutputfile] -> do
    --         if ignoreLines 
    --         then diff file1 file2 ignoreLines Nothing
    --         else diff file1 file2 False (Just optionOrOutputfile)
    --     [file1, file2] -> do
    --         diff file1 file2 False Nothing
    --     _ -> putStrLn "Usage : diff <file1_path> <file2_path> <-i|--ignore-lines> <outputFilePath>"
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

processaEntradas :: String -> String -> Char -> String -> IO ()
processaEntradas arquivo1 arquivo2 algoritmo output = diff algoritmo arquivo1 arquivo2 output

    
-- loop :: IO()
-- loop = do
--     input <- getLine
--     if input == "sair"
--         then putStrLn "Encerrando..."
--         else do
--             putStrLn ("Oi")
--             loop
    

