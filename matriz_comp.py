m = [7, 5, 2, 4, 9, 3, 1, 8, 6,
      3, 6, 8, 2, 5, 1, 7, 4, 9,
      9, 4, 1, 8, 6, 7, 3, 2, 5,
      1, 2, 3, 9, 7, 8, 6, 5, 4,
      5, 8, 9, 3, 4, 6, 2, 7, 1,
      4, 7, 6, 1, 2, 5, 8, 9, 3,
      8, 3, 5, 7, 1, 4, 9, 6, 2,
      2, 1, 4, 6, 8, 9, 5, 3, 7,
      6, 9, 7, 5, 3, 2, 4, 1, 8]
m = [9, 2, 3, 5, 4, 7, 1, 6, 8,
        6, 4, 7, 1, 9, 8, 5, 3, 2,
        5, 1, 8, 6, 3, 2, 7, 9, 4,
        4, 7, 5, 9, 2, 1, 3, 8, 6,
        1, 9, 6, 3, 8, 5, 4, 2, 7,
        3, 8, 2, 4, 7, 6, 9, 5, 1,
        7, 5, 4, 2, 6, 9, 8, 1, 3,
        8, 6, 9, 7, 1, 3, 2, 4, 5,
        2, 3, 1, 8, 5, 4, 6, 7, 9] # Nr. 125 -> 39.77
r = []

for i in range(81):
    e = []
    if i%27 <=8:
        e.append(-1)
    elif i//9 > 0:
        e.append(1) if m[i] > m[i-9] else e.append(0)
    else:
        e.append(-1)

    if i%3 == 2:
        e.append(-1)
    elif i%9 < 8:
        e.append(1) if m[i] > m[i+1] else e.append(0)
    else:
        e.append(-1)

    if i%27 >= 18:
        e.append(-1)
    elif i//9 < 8:
        e.append(1) if m[i] > m[i+9] else e.append(0)
    else:
        e.append(-1)
    
    if not i%3:
        e.append(-1)
    elif i%9 > 0:
        e.append(1) if m[i] > m[i-1] else e.append(0)
    else:
        e.append(-1)
    r.append(e)

print(r)