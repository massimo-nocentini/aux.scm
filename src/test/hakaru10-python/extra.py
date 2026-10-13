import math
from engine import *
r1_0 =  -2.66685511495;   r1_1 =  -24.4387534237;    r1_2 = -21.9698958928
r1_3 =  11.1667541262;    r1_4 =    3.13060547623;   r1_5 =   0.607771387771
r1_6 =  11.9400905721;    r1_7 =   31.4690115749;    r1_8 =  15.2346874070
r2_0 = -78.3359299449;    r2_1 = -142.046296688;     r2_2 = 137.519416416
r2_3 =  78.6994924154;    r2_4 =    4.16438922228;   r2_5 =  47.0668766060
r2_6 = 313.399215894;     r2_7 =  263.505074721;     r2_8 =  43.3400022514
r3_0 =  -2.12159572323e5; r3_1 =    2.30661510616e5; r3_2 =   2.74647644705e4
r3_3 =  -4.02621119975e4; r3_4 =   -2.29660729780e3; r3_5 =  -1.16328495004e5
r3_6 =  -1.46025937511e5; r3_7 =   -2.42357409629e4; r3_8 =  -5.70691009324e2
r4_0 = 0.279195317918525;  r4_1 = 0.4917317610505968;
r4_2 = 0.0692910599291889; r4_3 = 3.350343815022304
r4_4 = 6.012459259764103
def logGamma(x):
    if x<=0 or x>1e308: return float('inf')
    if x<1.5:
        a,b,c=(-math.log(x),x+1,x) if x<0.5 else (0,x,x-1)
        return a + c*((((r1_4*b+r1_3)*b+r1_2)*b+r1_1)*b+r1_0)/((((b+r1_8)*b+r1_7)*b+r1_6)*b+r1_5)
    if x<4: return (x-2)*((((r2_4*x+r2_3)*x+r2_2)*x+r2_1)*x+r2_0)/((((x+r2_8)*x+r2_7)*x+r2_6)*x+r2_5)
    if x<12: return ((((r3_4*x+r3_3)*x+r3_2)*x+r3_1)*x+r3_0)/((((x+r3_8)*x+r3_7)*x+r3_6)*x+r3_5)
    y=math.log(x); k=x*(y-1)-0.5*y+0.918938533204673
    if x>3e6: return k
    x1=1/x; x2=x1*x1
    return k + x1*((r4_2*x2+r4_1)*x2+r4_0)/((x2+r4_4)*x2+r4_3)
class Uniform:
    def __init__(s,lo,hi): s.lo=lo; s.hi=hi; s.l=math.log(1/(hi-lo))
    def logd(s,x): return s.l if s.lo<=x<=s.hi else NINF
    def sample(s,g): x,g=rR(s.lo,s.hi,g); return (x,s.l,g)
def normal_rng(mu,sd,g):
    (x,_),g1=marsaglia(g); return mu+sd*x,g1
def gamma_rng(shape,scl,g):
    if shape<1.0:
        g1v,g1=gamma_rng(shape+1,scl,g); w,g2=rR(0.0,1.0,g1); return scl*g1v*(w**(1/shape)),g2
    while True:
        d=shape-1/3; c=1/math.sqrt(9*d)
        v,g2=normal_rng(1,c,g)
        while not v>0.0: v,g2=normal_rng(1,c,g2)
        x=(v-1)/c; sqr=x*x; v3=v*v*v
        u,g3=rR(0.0,1.0,g2)
        if u<1.0-0.0331*(sqr*sqr) or math.log(u)<0.5*sqr+d*(1.0-v3+math.log(v3)): return scl*d*v3,g3
        g=g3
class Gamma:
    def __init__(s,a,b): s.a=a; s.b=b
    def logd(s,x):
        if x<0: return NINF
        return s.b*math.log(s.a)-s.b*x+(s.a-1)*math.log(x)-logGamma(s.a)
    def sample(s,g): x,g=gamma_rng(s.a,s.b,g); return x,s.logd(x),g
def beta_rng(a,b,g):
    if a<=1.0 and b<=1.0:
        while True:
            u,g1=rR(0.0,1.0,g); v,g2=rR(0.0,1.0,g1); x=u**(1/a); y=v**(1/b)
            if x+y<=1.0: return x/(x+y),g2
            g=g2
    ga,g1=gamma_rng(a,1,g); gb,g2=gamma_rng(b,1,g1); return ga/(ga+gb),g2
class Beta:
    def __init__(s,a,b): s.a=a; s.b=b
    def logd(s,x):
        f=logGamma(s.a+s.b)-logGamma(s.a)-logGamma(s.b)
        return f+(s.a-1)*math.log(x)+(s.b-1)*math.log(1-x)
    def sample(s,g): x,g=beta_rng(s.a,s.b,g); return x,s.logd(x),g
def momenta(l):
    n=float(len(l)); s=0.0
    for x in l: s=s+x
    av=s/n; acc=0.0
    for x in reversed(l): acc=(x-av)*(x-av)+acc
    return av,acc/(n-1)
def tabulate(step,l):
    q=1/float(len(l)); h={}
    for x in reversed(l):
        k=math.floor(x/step)*step; h[k]=q+h.get(k,0.0)
    return h
def kl(p,q):
    s=0.0
    for i in sorted(p): s=s+p[i]*math.log(p[i]/q[i])
    return s
