import os, sys; sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from hansei_py import *
_print=print
def show(x):
    return [(repr(p), v[1] if v[0]=='V' else 'C') for p,v in x] if isinstance(x,list) else x
def print(*a): _print(*[show(e) for e in a])
def tflip2_shared(d,f): v=flip(d,0.5); return v and v
def tflip2_noshared(d,f): return flip(d,0.5) and flip(d,0.5)
def talarm(d,f):
    e=flip(d,0.01); b=flip(d,0.1)
    if e: return flip(d,0.99) if b else flip(d,0.2)
    return flip(d,0.98) if b else flip(d,0.01)
def geometric(d,f,p):
    n=0
    while True:
        # reflect of loop n: [(p, C(->[(1,V n)])); (1-p, C(-> loop (n+1)))] : two levels per value
        if d([(p,0),(1.0-p,1)])==0:
            d([(1.0,None)]); return n
        n+=1
def drunk_coin(d,f):
    x=flip(d,0.5); lost=flip(d,0.9)
    if lost: f()
    return x
def dcoin_and(d,f,n):
    if n==1: return drunk_coin(d,f)
    return drunk_coin(d,f) and dcoin_and(d,f,n-1)
def tiblt2(d,f):
    x=d([(0.01,('a','b')),(0.02,('a','c')),(0.97,('d','e'))])
    if x[0]!='a': f()
    if x[1]=='b': return d([(0.9,True),(0.1,False)])
    if x[1]=='c': return d([(0.6,True),(0.1+0.3 if False else 0.4,False)])
    return d([(0.2,True),(0.8,False)])
if __name__ == '__main__':
    for kw in ({'xorvariant':True,'ndraw':3},{'xorvariant':False,'ndraw':3},{'xorvariant':True,'ndraw':2}):
        print("==",kw)
        S=lambda s: random_selector(s,**kw)
        print("rej flip2_shared s1", sample_rejection(S(1),100,tflip2_shared), "exp [(0.48,T);(0.52,F)]")
        print("imp flip2_shared s1", sample_importance(S(1),100,tflip2_shared))
        print("rej noshared s1", sample_rejection(S(1),100,tflip2_noshared), "exp 0.24/0.76")
        print("imp noshared s1", sample_importance(S(1),100,tflip2_noshared))
        print("rej talarm s1", sample_rejection(S(1),100,talarm), "exp 0.14/0.86")
        print("imp talarm s1", sample_importance(S(1),100,talarm))
        print("rej geom s17", sample_rejection(S(17),100,lambda d,f: geometric(d,f,0.85)), "exp 0.04 V2,0.17 V1,0.79 V0")
        print("imp geom s17 100", sample_importance(S(17),100,lambda d,f: geometric(d,f,0.85)))
        print("   exp [(0.000450000000000000096, V 4); (0.00247500000000000107, V 3); (0.0195749999999999813, V 2); (0.1275, V 1); (0.85, V 0)]")
        print("imp geom s17 1000", sample_importance(S(17),1000,lambda d,f: geometric(d,f,0.85)))
        print("rej tiblt2 s1", sample_rejection(S(1),100,tiblt2), "exp [(0.02,F)]")
        print("imp tiblt2 s1", sample_importance(S(1),100,tiblt2))
        print("rej dcoin s17 100", sample_rejection(S(17),100,lambda d,f: dcoin_and(d,f,10)), "exp [(0.05,F)]")
        print("rej dcoin s17 10000", sample_rejection(S(17),10000,lambda d,f: dcoin_and(d,f,10)), "exp [(0.0514,F)]")
        print("imp dcoin s17 100", sample_importance(S(17),100,lambda d,f: dcoin_and(d,f,10)), "exp [(0.052301965599999993, V false)]")
        print("imp dcoin s1 5000", sample_importance(S(1),5000,lambda d,f: dcoin_and(d,f,10)), "exp [(6.99999999999998406e-14, V true); (0.0526354788429301348, V false)]")
        print("imp dcoin s17 5000", sample_importance(S(17),5000,lambda d,f: dcoin_and(d,f,10)), "exp [(5.99999999999998706e-14, V true); (0.0526415594361701461, V false)]")
    print("exact dcoin", exact_reify(lambda d,f: dcoin_and(d,f,10)), "exp [(9.76562499999997764e-14, V true); (0.0526315789473632695, V false)]")
    print("explore geom Some 4", [(p,v if v[0]=='V' else 'C') for p,v in explore(4, reify0(lambda d,f: geometric(d,f,0.85)))])
