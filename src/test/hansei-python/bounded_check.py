import os, sys; sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from hansei_py import *
from hansei_check import dcoin_and
def bounded_explore(maxsize, choices):
    q={}  # key prob -> list of thunks (LIFO)
    low=0.0; high=0.0; size=0
    def process(explore, pc, lst):
        nonlocal low, high, size
        for p,(tag,x) in lst:
            if tag=='V':
                pe=pc*p; low=low+pe; high=high+pe
            elif explore:
                k=pc*p; q[k]=[x]+q.get(k,[]); size+=1
            else: high=high+pc*p
    process(True,1.0,choices)
    while size:
        k = max(q) if size<maxsize else min(q)
        explore = size<maxsize
        t=q[k][0]; rest=q[k][1:]
        if rest: q[k]=rest
        else: del q[k]
        size-=1
        process(explore,k,t())
    return (low,high)
def geometric_bounded(d,n,p):
    def expi(x,n):
        if n==0: return 1.0
        if n==1: return x
        return x*expi(x,n-1)
    norm=1.0-expi(1.0-p,n+1)
    lst=[]; pp=p/norm; i=0
    while i<=n: lst.append((pp,i)); pp=(1.0-p)*pp; i+=1
    return d(lst)
def m1(d,f):
    if geometric_bounded(d,7,0.85)>3: f()
print(bounded_explore(1, reify0(m1)), "exp (0.999494006159381776, 0.999494006159381776)")
def m2(d,f):
    if not dcoin_and(d,f,10): f()
for n,e in ((1,"(0., 1.)"),(3,"(0., 0.0499999999999999889)"),(5,"(9.76562499999997764e-14, 9.76562499999997764e-14)")):
    print(n, bounded_explore(n, reify0(m2)), "exp", e)
print("exact geom_bounded", [(repr(p),v[1]) for p,v in exact_reify(lambda d,f: geometric_bounded(d,7,0.85))])
