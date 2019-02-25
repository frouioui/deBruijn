#!/usr/bin/env python3

def debruijn(k, n):
    # Create a 0 array of size n
    v = [0 for _ in range(n)]
    l = 1
    r = []
    while True:
        if n % l == 0:
            r.extend(v[0:l])
        for i in range(l, n):
            v[i] = v[i-l]
        l = n
        while l > 0 and v[l-1] >= k-1:
            l-=1
        if l == 0:
            break
        v[l-1] += 1
    return r

print(debruijn(2,3))