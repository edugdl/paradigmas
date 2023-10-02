import Data.List
import Data.Maybe
import Debug.Trace

matrizComparacao :: [[Int]]
matrizComparacao = [[-1, 1, 1, -1], [-1, 1, 0, 0], [-1, -1, 0, 0], [-1, 0, 1, -1], [-1, 1, 1, 1], [-1, -1, 1, 0], [-1, 0, 0, -1], [-1, 1, 1, 1], [-1, -1, 0, 0], [0, 0, 0, -1], [1, 0, 1, 1], [1, -1, 1, 1], [0, 0, 0, -1], [0, 1, 0, 1], [0, -1, 0, 0], [1, 1, 1, -1], [0, 0, 1, 0], [1, -1, 1, 1], [1, 1, -1, -1], [0, 1, -1, 0], [0, -1, -1, 0], [1, 1, -1, -1], [1, 0, -1, 0], [1, -1, -1, 1], [0, 1, -1, -1], [0, 0, -1, 0], [0, -1, -1, 1], [-1, 0, 0, -1], [-1, 0, 0, 1], [-1, -1, 0, 1], [-1, 1, 1, -1], [-1, 0, 1, 0], [-1, -1, 1, 1], [-1, 1, 1, -1], [-1, 1, 0, 0], [-1, -1, 1, 0], [1, 0, 1, -1], [1, 0, 1, 1], [1, -1, 1, 1], [0, 0, 1, -1], [0, 0, 1, 1], [0, -1, 1, 1], [0, 0, 0, -1], [1, 1, 0, 1], [0, -1, 0, 0], [0, 0, -1, -1], [0, 1, -1, 1], [0, -1, -1, 0], [0, 0, -1, -1], [0, 0, -1, 1], [0, -1, -1, 1], [1, 0, -1, -1], [1, 1, -1, 1], [1, -1, -1, 0], [-1, 1, 1, -1], [-1, 0, 1, 0], [-1, -1, 1, 1], [-1, 1, 1, -1], [-1, 0, 0, 0], [-1, -1, 0, 1], [-1, 1, 1, -1], [-1, 1, 1, 0], [-1, -1, 0, 0], [0, 1, 0, -1], [0, 0, 0, 0], [0, -1, 0, 1], [0, 0, 1, -1], [1, 0, 1, 1], [1, -1, 1, 1], [0, 1, 1, -1], [0, 0, 1, 0], [1, -1, 0, 1], [1, 0, -1, -1], [1, 1, -1, 1], [1, -1, -1, 0], [0, 1, -1, -1], [0, 1, -1, 0], [0, -1, -1, 0], [0, 1, -1, -1], [0, 0, -1, 0], [1, -1, -1, 1]]

matrizPossibilidades :: [[Int]]
matrizPossibilidades =
    [ [j | j <- [(limite i [] 1)!!0 .. (10 - (limite i [] 0)!!0)]]
    | i <- [0..80]
    ]

trocaElemento :: Int -> a -> [a] -> [a]
trocaElemento posicao elemento lista =
    take posicao lista ++ [elemento] ++ drop (posicao + 1) lista

for :: Int -> Int -> [Int] -> [[Int]] -> Maybe [Int]
for indice posicao solucao possibilidades
    | indice < 0 = Nothing
    | otherwise = do
        let tentativa_atual = resolve ((possibilidades!!posicao)!!indice) posicao solucao possibilidades
        case tentativa_atual of
            Just tentativa -> Just tentativa
            Nothing -> for (indice - 1) posicao solucao possibilidades


resolve :: Int -> Int -> [Int] -> [[Int]] -> Maybe [Int]
resolve elemento posicao solucao possibilidades
    | elem elemento (obtemLinha (posicao `div` 9) solucao) = Nothing
    | not (compara elemento posicao solucao) = Nothing
    | posicao == 80 = Just (trocaElemento 80 elemento solucao)
    | otherwise =
        let
            solucao_atual = trocaElemento posicao elemento solucao
            tentativa_for = for (length (possibilidades !! (posicao + 1))-1) (posicao + 1) solucao_atual possibilidades
        in
            tentativa_for

elementoEstaNaLista :: Eq a => a -> [a] -> Bool
elementoEstaNaLista elemento lista = elemento `elem` lista

compara :: Int -> Int -> [Int] -> Bool
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
obtemLinha linha matriz =
    [matriz!!i | i <- [linha*9 .. ((linha+1)*9)-1]]


obtemColuna :: Int -> [Int] -> [Int]
obtemColuna coluna matriz =
    [matriz!!i | i <- [coluna, coluna+9 .. coluna+72]]    

obtemRegiao :: Int -> Int -> [Int] -> [Int]
obtemRegiao x_regiao y_regiao matriz =
    [[|j <- [0..2]]
    | i <- [0..2]
    ]

limite :: Int -> [Int] -> Int -> [Int]
limite posicao visitados lim
    | elem posicao visitados = 0:visitados
    | otherwise =
        let
            novos_visitados = posicao : visitados
            acima = posicao - 9
            direita = posicao + 1
            abaixo = posicao + 9
            esquerda = posicao - 1
            resAcima = if (matrizComparacao !! posicao !! 0 == lim) then limite acima visitados lim else 0 : novos_visitados
            resDireita = if (matrizComparacao !! posicao !! 1 == lim) then limite direita (drop 1 resAcima) lim else 0 : (drop 1 resAcima) 
            resAbaixo = if (matrizComparacao !! posicao !! 2 == lim) then limite abaixo (drop 1 resDireita) lim else 0 : (drop 1 resDireita)
            resEsquerda = if (matrizComparacao !! posicao !! 3 == lim) then limite esquerda (drop 1 resAbaixo) lim else 0 : (drop 1 resAbaixo)
        in
            (1 + resAcima!!0 + resDireita!!0 + resAbaixo!!0 + resEsquerda!!0):(drop 1 resEsquerda)

main :: IO ()
main = do
    let solucao = replicate 81 0
    let resultado = resolve ((matrizPossibilidades!!0)!!3) 0 solucao matrizPossibilidades
    case resultado of
        Just res -> print res
        Nothing -> putStrLn "Não há solução."