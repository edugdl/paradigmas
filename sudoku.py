import time


def matriz_possibilidades(comparacao):
    """
    Função que calcula a matriz de possibilidades para cada casa dado a matriz de comparação.
    
    @param: matriz comparação           Matriz de 81 listas de 4 elementos, que representa os sinais de maior e menor
    @return: matriz possibilidades      Matriz de 81 listas de 1 até 7 elementos, que representa os valores possíveis de inserção
    """
    matriz = []
    for i in range(81):
        linha = []
        for j in range(limite(i, comparacao, [], 1), 11-limite(i, comparacao, [], 0)):
            linha.append(j)
        matriz.append(linha)
    return matriz


def resolve(elemento, posicao, solucao, comparacao, possibilidades):
    """
    Função que aplica backtracking para resolver a matriz.
    
    @param: elemento                    Valor do elemento que será inserido no sudoku
    @param: posição                     Posição em que o elemento será posicionado
    @param: matriz solução              Matriz de 81 elementos que simula o sudoku, representando a tentativa de solução atual
    @param: matriz comparação           Matriz de 81 listas de 4 elementos, que representa os sinais de maior e menor
    @paran: matriz possibilidades       Matriz de 81 listas de 1 até 7 elementos, que representa os valores possíveis de inserção
    @return: término                    Se a solução atual é aceita (chegou ao fim) ou é impossível continuar a partir do valor atual 
    """
    if repete_linha(elemento, posicao, solucao):
        return False
    elif repete_coluna(elemento, posicao, solucao):
        return False
    elif repete_regiao(elemento, posicao, solucao):
        return False
    elif not compara(elemento, posicao, solucao, comparacao):
        return False
    solucao[posicao] = elemento
    if posicao == 80: return True
    for t in possibilidades[posicao+1]:
        if resolve(t, posicao+1, solucao, comparacao, possibilidades): return True
    solucao[posicao] = 0
    return False


def compara(elemento, posicao, matriz, comparacao):
    """
    Função que verifica se o elemento pode ser inserido em uma posição dado suas comparações de maior e menor.
    Faz a verificação com os elementos acima, à direita, abaixo e à esquerda, nessa ordem.
    Caso qualquer comparação falhe, o resultado é falso. Caso contrário, verdadeiro.
    
    @param: elemento                    Valor do elemento que será inserido no sudoku
    @param: posição                     Posição em que o elemento será posicionado
    @param: matriz                      A matriz de 81 elementos, que simula o sudoku
    @param: matriz comparação           Matriz de 81 listas de 4 elementos, que representa os sinais de maior e menor
    @return: condição                   Se é possível inserir o elemento em dada posição
    """
    if comparacao[posicao][0] == 1:
        if elemento < matriz[posicao - 9] and matriz[posicao - 9]: return False
    elif comparacao[posicao][0] == 0:
        if elemento > matriz[posicao - 9] and matriz[posicao - 9]: return False

    if comparacao[posicao][1] == 1:
        if elemento < matriz[posicao + 1] and matriz[posicao + 1]: return False
    elif comparacao[posicao][1] == 0:
        if elemento > matriz[posicao + 1] and matriz[posicao + 1]: return False

    if comparacao[posicao][2] == 1:
        if elemento < matriz[posicao + 9] and matriz[posicao + 9]: return False
    elif comparacao[posicao][2] == 0:
        if elemento > matriz[posicao + 9] and matriz[posicao + 9]: return False
        
    if comparacao[posicao][3] == 1:
        if elemento < matriz[posicao - 1] and matriz[posicao - 1]: return False
    elif comparacao[posicao][3] == 0:
        if elemento > matriz[posicao - 1] and matriz[posicao - 1]: return False
    
    return True


def repete_linha(elemento, posicao, matriz):
    """
    Função que verifica se o elemento pode ser inserido em uma posição dado elementos presentes na sua linha.
    Caso o elemento já exista na linha, o resultado é verdadeiro. Caso contrário, falso.
    
    @param: elemento                    Valor do elemento que será inserido no sudoku
    @param: posição                     Posição em que o elemento será posicionado
    @param: matriz                      A matriz de 81 elementos, que simula o sudoku
    @return: condição                   Se é possível inserir o elemento em dada posição
    """
    for i in range(9):
        if matriz[i + (posicao//9)*9] == elemento: return True
    return False


def repete_coluna(elemento, posicao, matriz):
    """
    Função que verifica se o elemento pode ser inserido em uma posição dado elementos presentes na sua coluna.
    Caso o elemento já exista na coluna, o resultado é verdadeiro. Caso contrário, falso.
    
    @param: elemento                    Valor do elemento que será inserido no sudoku
    @param: posição                     Posição em que o elemento será posicionado
    @param: matriz                      A matriz de 81 elementos, que simula o sudoku
    @return: condição                   Se é possível inserir o elemento em dada posição
    """
    for i in range(9):
        if matriz[i*9 + posicao%9] == elemento: return True
    return False


def repete_regiao(elemento, posicao, matriz):
    """
    Função que verifica se o elemento pode ser inserido em uma posição dado elementos presentes na sua região.
    Caso o elemento já exista na região, o resultado é verdadeiro. Caso contrário, falso.
    
    @param: elemento                    Valor do elemento que será inserido no sudoku
    @param: posição                     Posição em que o elemento será posicionado
    @param: matriz                      A matriz de 81 elementos, que simula o sudoku
    @return: condição                   Se é possível inserir o elemento em dada posição
    """
    linha = (posicao//27)*3
    coluna = ((posicao%9)//3) * 3
    for i in range(linha, linha+3):
        for j in range(coluna, coluna+3):
            if matriz[i*9 + j] == elemento: return True
    return False


def limite(posicao, comparacao, visitados, comp):
    """
    Função que aplica uma DFS (busca em largura) pelos símbolos de '<' ou '>'.
    Dessa maneira é possível verificar a quantidade de elementos que a célula deve ser menor, ou maior.
    Dada 3 células do sudoku m, n e o. Se m > n > o, m não pode ser 1 nem 2, apenas valores > 3 satisfazem a inequação.
    Assim, verificamos os limites superiores e inferiores de uma célula no sudoku, reduzindo a complexidade do algoritmo.
        
    @param: posição                     Posição em que o elemento será posicionado
    @param: matriz comparação           Matriz de 81 listas de 4 elementos, que representa os sinais de maior e menor
    @param: elementos visitados         Lista que contém todos os elementos já visitados na busca em profundidade
    @param: comparação da busca         Admite apenas duas entrada = 0 ou 1, pois representa qual busca deseja se fazer, pelo símbolo '<' ou '>'
    @return: total                      Quantidade de células do sudokus visitadas a partir do elemento de comparação
    """
    total = 1
    visitados.append(posicao)
    if comparacao[posicao][0] == comp and posicao-9 not in visitados:
        total += limite(posicao-9, comparacao, visitados, comp)

    if comparacao[posicao][1] == comp and posicao+1 not in visitados:
        total += limite(posicao+1, comparacao, visitados, comp)
        
    if comparacao[posicao][2] == comp and posicao+9 not in visitados:
        total += limite(posicao+9, comparacao, visitados, comp)
        
    if comparacao[posicao][3] == comp and posicao-1 not in visitados:
        total += limite(posicao-1, comparacao, visitados, comp)
    
    return total


def mostrar_matriz(matriz):
    """
    Função com o propósito de formatar a saída da matriz como um sudoku = 9 regiões 3x3.\n
    [] [] []\n
    [] [] []\n
    [] [] []
        
    @param: matriz                      A matriz de 81 elementos, que simula o sudoku
    @return: void
    """
    print()
    for i in range(len(matriz)):
        if not i % 27 and i: 
            print("\n\n")
        elif not i % 9: 
            print()
        elif not i % 3: 
            print("   ", end="")
        print(matriz[i], end=" ")
    print()


def main():
    """
    - Função que integra todas as demais.\n
    - Inicia definindo a matriz que simula o sudoku com 0 em suas 81 posições\n
    - Define-se a matriz de comparação, simulando as comparações do Vergleich\n
    - Cria-se a matriz de possibilidades de inserção nas posições do sudoku\n
    - O sudoku é resolvido, e calcula-se seu tempo de execução\n
    - O resultado é mostrado na tela, juntamente com o tempo total 
    """
    solucao = [0 for _ in range(81)]
    matriz_comparacao = [[-1, 1, 1, -1], [-1, 1, 0, 0], [-1, -1, 0, 0], [-1, 0, 1, -1], [-1, 1, 1, 1], [-1, -1, 1, 0], [-1, 0, 0, -1], [-1, 1, 1, 1], [-1, -1, 0, 0],
                         [0, 0, 0, -1], [1, 0, 1, 1], [1, -1, 1, 1], [0, 0, 0, -1], [0, 1, 0, 1], [0, -1, 0, 0], [1, 1, 1, -1], [0, 0, 1, 0], [1, -1, 1, 1],
                         [1, 1, -1, -1], [0, 1, -1, 0], [0, -1, -1, 0], [1, 1, -1, -1], [1, 0, -1, 0], [1, -1, -1, 1], [0, 1, -1, -1], [0, 0, -1, 0], [0, -1, -1, 1],
                         [-1, 0, 0, -1], [-1, 0, 0, 1], [-1, -1, 0, 1], [-1, 1, 1, -1], [-1, 0, 1, 0], [-1, -1, 1, 1], [-1, 1, 1, -1], [-1, 1, 0, 0], [-1, -1, 1, 0],
                         [1, 0, 1, -1], [1, 0, 1, 1], [1, -1, 1, 1], [0, 0, 1, -1], [0, 0, 1, 1], [0, -1, 1, 1], [0, 0, 0, -1], [1, 1, 0, 1], [0, -1, 0, 0],
                         [0, 0, -1, -1], [0, 1, -1, 1], [0, -1, -1, 0], [0, 0, -1, -1], [0, 0, -1, 1], [0, -1, -1, 1], [1, 0, -1, -1], [1, 1, -1, 1], [1, -1, -1, 0],
                         [-1, 1, 1, -1], [-1, 0, 1, 0], [-1, -1, 1, 1], [-1, 1, 1, -1], [-1, 0, 0, 0], [-1, -1, 0, 1], [-1, 1, 1, -1], [-1, 1, 1, 0], [-1, -1, 0, 0], 
                         [0, 1, 0, -1], [0, 0, 0, 0], [0, -1, 0, 1], [0, 0, 1, -1], [1, 0, 1, 1], [1, -1, 1, 1], [0, 1, 1, -1], [0, 0, 1, 0], [1, -1, 0, 1],
                         [1, 0, -1, -1], [1, 1, -1, 1], [1, -1, -1, 0], [0, 1, -1, -1], [0, 1, -1, 0], [0, -1, -1, 0], [0, 1, -1, -1], [0, 0, -1, 0], [1, -1, -1, 1]]
    possibilidades = matriz_possibilidades(matriz_comparacao)
    
    inicio = time.time()
    for t in possibilidades[0]:
        if resolve(t, 0, solucao, matriz_comparacao, possibilidades): 
            break
    fim = time.time()
    
    tempo_de_execucao = fim - inicio
    
    mostrar_matriz(solucao)
    print(f"O código levou {tempo_de_execucao} segundos para ser executado.")


if __name__ == "__main__":
    main()