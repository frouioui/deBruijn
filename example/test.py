#!/usr/bin/env python3

def debruijn(k, n):
    v = [0 for _ in range(n)]
    l = 1
    r = []
    m = 0
    while m < 3:
        m += 1
        if n % l == 0:
            r.extend(v[0:l])
            print(r)
        for i in range(l, n):
            v[i] = v[i-l]
        l = n
        while l > 0 and v[l-1] >= k-1:
            l-=1
        if l == 0:
            break
        v[l-1] += 1
    return r

print(debruijn(2,2))