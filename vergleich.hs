import Data.Maybe
import Debug.Trace


matrizComparacao :: [[Int]]
    ---
    --- @return: matriz comparação           Matriz de 81 listas de 4 elementos, que representa os sinais de maior e menor
    ---
matrizComparacao = [[-1, 1, 1, -1], [-1, 0, 0, 0], [-1, -1, 0, 1], [-1, 1, 1, -1], [-1, 0, 0, 0], [-1, -1, 0, 1], [-1, 0, 0, -1], [-1, 0, 1, 1], [-1, -1, 1, 1], [0, 1, 1, -1], [1, 0, 1, 0], [1, -1, 0, 1], [0, 0, 0, -1], [1, 1, 1, 1], [1, -1, 1, 0], [1, 1, 0, -1], [0, 1, 0, 0], [0, -1, 0, 0], [0, 1, -1, -1], [0, 0, -1, 0], [1, -1, -1, 1], [1, 1, -1, -1], [0, 1, -1, 0], [0, -1, -1, 0], [1, 0, -1, -1], [1, 1, -1, 1], [1, -1, -1, 0], [-1, 0, 1, -1], [-1, 1, 0, 1], [-1, -1, 0, 0], [-1, 1, 1, -1], [-1, 1, 0, 0], [-1, -1, 0, 0], [-1, 0, 0, -1], [-1, 1, 1, 1], [-1, -1, 0, 0], [0, 0, 0, -1], [1, 1, 1, 1], [1, -1, 1, 0], [0, 0, 0, -1], [1, 1, 1, 1], [1, -1, 0, 0], [1, 1, 0, -1], [0, 0, 0, 0], [1, -1, 1, 1], [1, 0, -1, -1], [0, 1, -1, 1], [0, -1, -1, 0], [1, 0, -1, -1], [0, 1, -1, 1], [1, -1, -1, 0], [1, 1, -1, -1], [1, 1, -1, 0], [0, -1, -1, 0], [-1, 1, 0, -1], [-1, 1, 0, 0], [-1, -1, 0, 0], [-1, 0, 0, -1], [-1, 0, 1, 1], [-1, -1, 1, 1], [-1, 1, 1, -1], [-1, 0, 0, 0], [-1, -1, 0, 1], [1, 1, 1, -1], [1, 0, 1, 0], [1, -1, 1, 1], [1, 1, 0, -1], [0, 0, 0, 0], [0, -1, 0, 1], [0, 0, 0, -1], [1, 0, 0, 1], [1, -1, 0, 1], [0, 0, -1, -1], [0, 1, -1, 1], [0, -1, -1, 0], [1, 1, -1, -1], [1, 1, -1, 0], [1, -1, -1, 0], [1, 0, -1, -1], [1, 0, -1, 1], [1, -1, -1, 1]]

matrizPossibilidades :: [[Int]]
    ---
    -- Função que calcula a matriz de possibilidades para cada casa dado a matriz de comparação.
    
    -- @return: matriz possibilidades   Matriz de 81 listas de 1 até 7 elementos, que representa os valores possíveis de inserção
    ---
matrizPossibilidades =
    [ [j | j <- [(limite i [] 1)!!0 .. (10 - (limite i [] 0)!!0)]]
    | i <- [0..80]
    ]


trocaElemento :: Int -> a -> [a] -> [a]
    ---
    -- Função que substitui um elemento por outro em uma dada lista.
    -- @param: posição                     Posição que será alterada
    -- @param: elemento                    Elemento que será inserido
    -- @param: lista                       Lista que será modificada
    -- @return: lista substituida          Nova lista com o valor substituído
    ---
trocaElemento posicao elemento lista =
    take posicao lista ++ [elemento] ++ drop (posicao + 1) lista


for :: Int -> Int -> [Int] -> [[Int]] -> Maybe [Int]
    ---
    -- Função que simula um for iterando pelas possibilidades da posição.
    -- Caso é encontrado um valor que pode ser inserido, retorna-o.
    -- Caso contrário, passa para a próxima possibilidade, até ser encontrada, ou retornar falso ao nenhuma ser possível.

    -- @param: índice                      Índice do for
    -- @param: posição                     Posição que será alterada
    -- @param: matriz solução              Matriz de 81 elementos que simula o sudoku, representando a tentativa de solução atual
    -- @param: matriz possibilidades       Matriz de 81 listas de 1 até 7 elementos, que representa os valores possíveis de inserção
    -- @return: tentativa                  Valor encontrado, ou itera novamente pelo for
    ---
for indice posicao solucao possibilidades
    | indice < 0 = Nothing
    | otherwise = do
        let tentativa_atual = resolve ((possibilidades !! posicao) !! indice) posicao solucao possibilidades
        case tentativa_atual of
            Just tentativa -> Just tentativa
            Nothing -> for (indice - 1) posicao solucao possibilidades


resolve :: Int -> Int -> [Int] -> [[Int]] -> Maybe [Int]
    ---
    -- Função que aplica backtracking para resolver a matriz.
    
    -- @param: elemento                    Valor do elemento que será inserido no sudoku
    -- @param: posição                     Posição em que o elemento será posicionado
    -- @param: matriz solução              Matriz de 81 elementos que simula o sudoku, representando a tentativa de solução atual
    -- @paran: matriz possibilidades       Matriz de 81 listas de 1 até 7 elementos, que representa os valores possíveis de inserção
    -- @return: término                    Se a solução atual é aceita (chegou ao fim), se é impossível continuar a partir do valor atual ou um novo for 
    ---
resolve elemento posicao solucao possibilidades
    | elem elemento (obtemLinha (posicao `div` 9) solucao) = Nothing
    | elem elemento (obtemColuna (posicao `mod` 9) solucao) = Nothing
    | elem elemento (obtemRegiao (posicao `div` 27) ((posicao `mod` 9) `div` 3) solucao) = Nothing
    | not (compara elemento posicao solucao) = Nothing
    | posicao == 80 = Just (trocaElemento 80 elemento solucao)
    | otherwise =
        let
            solucao_atual = trocaElemento posicao elemento solucao
            tentativa_for = for (length (possibilidades !! (posicao + 1))-1) (posicao + 1) solucao_atual possibilidades
        in
            tentativa_for

compara :: Int -> Int -> [Int] -> Bool
    ---
    -- Função que verifica se o elemento pode ser inserido em uma posição dado suas comparações de maior e menor.
    -- Faz a verificação com os elementos acima, à direita, abaixo e à esquerda, nessa ordem.
    -- Caso qualquer comparação falhe, o resultado é falso. Caso contrário, verdadeiro.
    
    -- @param: elemento                    Valor do elemento que será inserido no sudoku
    -- @param: posição                     Posição em que o elemento será posicionado
    -- @param: matriz                      A matriz de 81 elementos, que simula o sudoku
    -- @return: condição                   Se é possível inserir o elemento em dada posição
    ---
compara elemento posicao matriz =
    let
        acima = posicao - 9
        direita = posicao + 1
        abaixo = posicao + 9
        esquerda = posicao - 1
        comparaPosicao c p =
            case c of
                1 -> if matriz!!p /= 0 then matriz!!p < elemento else True
                0 -> if matriz!!p /= 0 then matriz!!p > elemento else True
                _ -> True
    in
        comparaPosicao (matrizComparacao !! posicao !! 0) acima &&
        comparaPosicao (matrizComparacao !! posicao !! 1) direita &&
        comparaPosicao (matrizComparacao !! posicao !! 2) abaixo &&
        comparaPosicao (matrizComparacao !! posicao !! 3) esquerda


obtemLinha :: Int -> [Int] -> [Int]
    ---
    -- Função que retorna uma das 9 linhas existentes no sudoku

    -- @param: linha                       Qual linha deseja-se obter
    -- @param: matriz                      A matriz de 81 elementos, que simula o sudoku
    -- @return: linha_matriz               A linha da matriz
    ---
obtemLinha linha matriz =
    [matriz!!i | i <- [linha*9 .. ((linha+1)*9)-1]]


obtemColuna :: Int -> [Int] -> [Int]
    ---
    -- Função que retorna uma das 9 colunas existentes no sudoku

    -- @param: coluna                      Qual coluna deseja-se obter
    -- @param: matriz                      A matriz de 81 elementos, que simula o sudoku
    -- @return: coluna_matriz              A coluna da matriz
    ---
obtemColuna coluna matriz =
    [matriz!!i | i <- [coluna, coluna+9 .. coluna+72]]    


obtemRegiao :: Int -> Int -> [Int] -> [Int]
    ---
    -- Função que retorna uma das 9 regiões existentes no sudoku

    -- @param: região                      Qual região deseja-se obter
    -- @param: matriz                      A matriz de 81 elementos, que simula o sudoku
    -- @return: região_matriz              A região da matriz
    ---
obtemRegiao x_regiao y_regiao matriz =
    [matriz!!(((x_regiao*27)+(i*9))+(y_regiao*3)+j)|i<-[0..2],j<-[0..2]]


limite :: Int -> [Int] -> Int -> [Int]
    ---
    -- Função que aplica uma DFS (busca em largura) pelos símbolos de '<' ou '>'.
    -- Dessa maneira é possível verificar a quantidade de elementos que a célula deve ser menor, ou maior.
    -- Dada 3 células do sudoku m, n e o. Se m > n > o, m não pode ser 1 nem 2, apenas valores > 3 satisfazem a inequação.
    -- Assim, verificamos os limites superiores e inferiores de uma célula no sudoku, reduzindo a complexidade do algoritmo.
        
    -- @param: posição                     Posição em que o elemento será posicionado
    -- @param: elementos visitados         Lista que contém todos os elementos já visitados na busca em profundidade
    -- @param: comparação da busca         Admite apenas duas entrada = 0 ou 1, pois representa qual busca deseja se fazer, pelo símbolo '<' ou '>'
    -- @return: [total, novos visitados]   A quantidade e os valores das células do sudokus visitadas a partir do elemento de comparação
    ---
limite posicao visitados comp
    | elem posicao visitados = 0:visitados
    | otherwise =
        let
            novos_visitados = posicao : visitados
            acima = posicao - 9
            direita = posicao + 1
            abaixo = posicao + 9
            esquerda = posicao - 1
            resAcima = if (matrizComparacao !! posicao !! 0 == comp) then limite acima novos_visitados comp else 0 : novos_visitados
            resDireita = if (matrizComparacao !! posicao !! 1 == comp) then limite direita (drop 1 resAcima) comp else 0 : (drop 1 resAcima) 
            resAbaixo = if (matrizComparacao !! posicao !! 2 == comp) then limite abaixo (drop 1 resDireita) comp else 0 : (drop 1 resDireita)
            resEsquerda = if (matrizComparacao !! posicao !! 3 == comp) then limite esquerda (drop 1 resAbaixo) comp else 0 : (drop 1 resAbaixo)
        in
            (1 + resAcima!!0 + resDireita!!0 + resAbaixo!!0 + resEsquerda!!0):(drop 1 resEsquerda)


mostrarSudokuFormatado :: [Int] -> String
    ---
    -- Função com o propósito de formatar a saída da matriz como um sudoku = 9 regiões 3x3.\n
    -- [] [] []\n
    -- [] [] []\n
    -- [] [] []
        
    -- @param: matriz                      A matriz de 81 elementos, que simula o sudoku
    -- @return: matriz_formatada           Uma string que contém a matriz no formato de sudoku
    ---
mostrarSudokuFormatado solucao = concat [formatarLinha i | i <- [0..80]]
  where
    formatarLinha i
      | i > 0 && i `mod` 27 == 0 = "\n\n" ++ formatarElemento i
      | i > 0 && i `mod` 9 == 0  = "\n" ++ formatarElemento i
      | i > 0 && i `mod` 3 == 0  = "   " ++ formatarElemento i
      | otherwise                = formatarElemento i
      where
        formatarElemento i' = show (solucao !! i') ++ " "


main :: IO ()
    ---
    -- - Função que integra todas as demais\n
    -- - Inicia definindo a matriz que simula o sudoku com 0 em suas 81 posições\n
    -- - Cria-se a matriz de possibilidades de inserção nas posições do sudoku\n
    -- - O sudoku é resolvido\n
    -- - O resultado é mostrado na tela
    ---
main = do
    let solucao = replicate 81 0
    let possibilidades = matrizPossibilidades
    let resultado = for (length (possibilidades !! 0)-1) 0 solucao possibilidades
    case resultado of
        Just res -> putStrLn (mostrarSudokuFormatado res)
        Nothing -> putStrLn "Não há solução."
