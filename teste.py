solucao = [i for i in range(1, 82)]
for i in range(len(solucao)):
    if not i % 27 and i: print("\n\n")
    elif not i % 9: print()
    elif not i % 3: print("   ", end="")
    print(solucao[i], end=" ")