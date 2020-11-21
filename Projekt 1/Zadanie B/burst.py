n = 3
arr = ['a','b','c','c','c','d','e','e']
from collections import Counter
def sa(n,a):
    count = 1
    for i in range(len(a)-2):
        if a[i] == a[i+1]:
            count += 1
        if count == n:
            for j in range(0,n-1):
                a.pop(i-j)
    return a
        

result = sa(n,arr)

for i in result:
    print(i)