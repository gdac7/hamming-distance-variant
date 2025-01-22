import GetDiffLines (editDistancePerLine, averageEditDistancePerLine, hammingDistancePerLine, averageHammingPerLine, getMissingLines)
import FileHandler (writeResult, readFiles)
import System.IO
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
    let (missingLines, fileMissing) = getMissingLines lines1 lines2
    --let finalResultLine = zip dist (map roundTo2 averagePerLine)
    -- Usar show na linha abaixo causa exibição de números em notação científica
    let linesInfo = missingInfo fileMissing missingLines file1 file2
            

    let result = unlines [ "Diff = " ++ show d ++ ", Media = " ++  printf "%.2f" avg | (d, avg) <- zip dist (map roundTo2 averagePerLine)] ++ "\n" ++ linesInfo


    if null output
        then putStrLn result
        else do
            writeFile output result



            

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
    

