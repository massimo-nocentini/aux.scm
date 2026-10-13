import math
# StdGen of random-1.0.1.1 (System/Random.hs): mk is mkStdGen, nxt is next, ival is
# randomIvalInteger and rR is randomR on Double. ival_11 is the random-1.1 variant of
# randomIvalInteger, kept for comparison and unused.
def mk(s):
    s &= 0x7fffffff
    q, s1 = divmod(s, 2147483562)
    return (s1+1, q % 2147483398 + 1)
def quot(a,b):
    q = abs(a)//abs(b); return q if (a>=0)==(b>0) else -q
def nxt(g):
    s1,s2=g
    k=quot(s1,53668); s1p=40014*(s1-k*53668)-k*12211
    if s1p<0: s1p+=2147483563
    k2=quot(s2,52774); s2p=40692*(s2-k2*52774)-k2*3791
    if s2p<0: s2p+=2147483399
    z=s1p-s2p
    if z<1: z+=2147483562
    return z,(s1p,s2p)
B=2147483562
def ival_11(l,h,g):
    if l>h: l,h=h,l
    k=h-l+1; mag=1; v=0; tgt=k*1000
    while mag<tgt:
        x,g=nxt(g); v=v*B+(x-1); mag*=B
    return l+v%k, g
def rdouble(g):
    x,g=ival(-2**63,2**63-1,g)
    return float(x & (2**53-1))/float(2**53), g
def rR(l,h,g):
    if l>h: l,h=h,l
    c,g=rdouble(g)
    return 2.0*(0.5*l + c*(0.5*h-0.5*l)), g
def ilog(b,i): return 1 if i<b else 1+ilog(b,i//b)
def ival(l,h,g):
    if l>h: l,h=h,l
    k=h-l+1; b=2147483561; n=ilog(b,k); acc=1
    for _ in range(n):
        x,g=nxt(g); acc=x+acc*b
    return l+acc%k, g
