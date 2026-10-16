import os, sys; sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from ocamlrandom import R as cls
def rint(s,n):
    while True:
        r=s.bits(); v=r%n
        if r-v <= 0x3FFFFFFF-n+1: return v
for seed in (1,17):
    for n in (10,1000):
        s=cls(seed,xorvariant=True,ndraw=3)
        print(seed,n,[rint(s,n) for _ in range(5)])
