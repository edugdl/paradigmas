import time

def matriz_possibilidades(comparacao):
    m = []
    for i in range(81):
        e = []
        for j in range(limite(i, comparacao, [], 1), 11-limite(i, comparacao, [], 0)):
            e.append(j)
        m.append(e)
    return m


def resolve(elemento, posicao, solucao, comparacao, possibilidades):
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
    for i in range(9):
        if matriz[i + (posicao//9)*9] == elemento: return True
    return False

def repete_coluna(elemento, posicao, matriz):
    for i in range(9):
        if matriz[i*9 + posicao%9] == elemento: return True
    return False

def repete_regiao(elemento, posicao, matriz):
    linha = (posicao//27)*3
    coluna = ((posicao%9)//3) * 3
    for i in range(linha, linha+3):
        for j in range(coluna, coluna+3):
            if matriz[i*9 + j] == elemento: return True
    return False

def limite(posicao, comparacao, visitados, lim):
    total = 1
    visitados.append(posicao)
    if comparacao[posicao][0] == lim and posicao-9 not in visitados:
        total += limite(posicao-9, comparacao, visitados, lim)

    if comparacao[posicao][1] == lim and posicao+1 not in visitados:
        total += limite(posicao+1, comparacao, visitados, lim)
        
    if comparacao[posicao][2] == lim and posicao+9 not in visitados:
        total += limite(posicao+9, comparacao, visitados, lim)
        
    if comparacao[posicao][3] == lim and posicao-1 not in visitados:
        total += limite(posicao-1, comparacao, visitados, lim)
    
    return total

matriz_comparacao = [[-1, 1, 1, -1], [-1, 1, 0, 0], [-1, -1, 0, 0], [-1, 0, 1, -1], [-1, 1, 1, 1], [-1, -1, 1, 0], [-1, 0, 0, -1], [-1, 1, 1, 1], [-1, -1, 0, 0], [0, 0, 0, -1], [1, 0, 1, 1], [1, -1, 1, 1], [0, 0, 0, -1], [0, 1, 0, 1], [0, -1, 0, 0], [1, 1, 1, -1], [0, 0, 1, 0], [1, -1, 1, 1], [1, 1, -1, -1], [0, 1, -1, 0], [0, -1, -1, 0], [1, 1, -1, -1], [1, 0, -1, 0], [1, -1, -1, 1], [0, 1, -1, -1], [0, 0, -1, 0], [0, -1, -1, 1], [-1, 0, 0, -1], [-1, 0, 0, 1], [-1, -1, 0, 1], [-1, 1, 1, -1], [-1, 0, 1, 0], [-1, -1, 1, 1], [-1, 1, 1, -1], [-1, 1, 0, 0], [-1, -1, 1, 0], [1, 0, 1, -1], [1, 0, 1, 1], [1, -1, 1, 1], [0, 0, 1, -1], [0, 0, 1, 1], [0, -1, 1, 1], [0, 0, 0, -1], [1, 1, 0, 1], [0, -1, 0, 0], [0, 0, -1, -1], [0, 1, -1, 1], [0, -1, -1, 0], [0, 0, -1, -1], [0, 0, -1, 1], [0, -1, -1, 1], [1, 0, -1, -1], [1, 1, -1, 1], [1, -1, -1, 0], [-1, 1, 1, -1], [-1, 0, 1, 0], [-1, -1, 1, 1], [-1, 1, 1, -1], [-1, 0, 0, 0], [-1, -1, 0, 1], [-1, 1, 1, -1], [-1, 1, 1, 0], [-1, -1, 0, 0], [0, 1, 0, -1], [0, 0, 0, 0], [0, -1, 0, 1], [0, 0, 1, -1], [1, 0, 1, 1], [1, -1, 1, 1], [0, 1, 1, -1], [0, 0, 1, 0], [1, -1, 0, 1], [1, 0, -1, -1], [1, 1, -1, 1], [1, -1, -1, 0], [0, 1, -1, -1], [0, 1, -1, 0], [0, -1, -1, 0], [0, 1, -1, -1], [0, 0, -1, 0], [1, -1, -1, 1]]

solucao = [0 for _ in range(81)]
inicio = time.time()
possibilidades = matriz_possibilidades(matriz_comparacao)
print(possibilidades[0])
for t in possibilidades[0]:
    print(t)
    if resolve(t,0,solucao, matriz_comparacao, possibilidades): break
fim = time.time()
tempo_de_execucao = fim - inicio
print(f"O código levou {tempo_de_execucao} segundos para ser executado.")

for i in range(len(solucao)):
    if not i % 27 and i: print("\n\n")
    elif not i % 9: print()
    elif not i % 3: print("   ", end="")
    print(solucao[i], end=" ")