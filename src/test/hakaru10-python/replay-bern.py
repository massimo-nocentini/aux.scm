import sys, math
sys.path.insert(0, sys.argv[1])
from stdgen import *
# pbern: mcmC 10 (dist bern 0.4)
def bern_sample(p,g):
    t,g=rR(0.0,1.0,g); lT=math.log(p); lF=math.log(1-p)
    return (True,lT,g) if t<=p else (False,lF,g)
g=mk(17)
v,ll,g=bern_sample(0.4,g)
out=[]
for i in range(10):
    out.append(v)
    _,g=ival(0,0,g)
    v2,ll2,g=bern_sample(0.4,g)
    lldiff=0+ll2-ll; fb=ll2-ll; r=lldiff-fb
    if r>=0: acc=True
    else:
        u,g=rR(0.0,1.0,g); acc=u<math.exp(r)
    if acc: v,ll=v2,ll2
print("pbern",out, sum(out))
# biased coin True False
def beta11(g):
    while True:
        u,g=rR(0.0,1.0,g); w,g=rR(0.0,1.0,g)
        x=u**1.0; y=w**1.0
        if x+y<=1.0: return x/(x+y),g
def lb(p,x): return math.log(p) if x else math.log(1-p)
r1_0 =  -2.66685511495;   r1_1 =  -24.4387534237;    r1_2 = -21.9698958928
r1_3 =  11.1667541262;    r1_4 =    3.13060547623;   r1_5 =   0.607771387771
r1_6 =  11.9400905721;    r1_7 =   31.4690115749;    r1_8 =  15.2346874070
r2_0 = -78.3359299449;    r2_1 = -142.046296688;     r2_2 = 137.519416416
r2_3 =  78.6994924154;    r2_4 =    4.16438922228;   r2_5 =  47.0668766060
r2_6 = 313.399215894;     r2_7 =  263.505074721;     r2_8 =  43.3400022514
def lgam(x):
    if x<1.5:
        a,b,c=(-math.log(x),x+1,x) if x<0.5 else (0,x,x-1)
        return a + c*((((r1_4*b+r1_3)*b+r1_2)*b+r1_1)*b+r1_0)/((((b+r1_8)*b+r1_7)*b+r1_6)*b+r1_5)
    if x<4:
        return (x-2)*((((r2_4*x+r2_3)*x+r2_2)*x+r2_1)*x+r2_0)/((((x+r2_8)*x+r2_7)*x+r2_6)*x+r2_5)
    raise
fac=lgam(2.0)-lgam(1.0)-lgam(1.0)
def bld(x): return fac+0.0*math.log(x)+0.0*math.log(1-x)
g=mk(17)
b,g=beta11(g); llb=bld(b); lo1=lb(b,True); lo2=lb(b,False)
out=[]
for i in range(10000):
    out.append(b)
    _,g=ival(0,0,g)
    b2,g=beta11(g); llb2=bld(b2)
    llc=0.0+llb2-llb
    n1=lb(b2,True); llc=llc+n1-lo1
    n2=lb(b2,False); llc=llc+n2-lo2
    fb=llb2-llb
    if math.isinf(llc): acc=False
    else:
        r=llc-fb
        if r>=0: acc=True
        else:
            u,g=rR(0.0,1.0,g); acc=u<math.exp(r)
    if acc: b,llb,lo1,lo2=b2,llb2,n1,n2
xs=out[1000:]
n=float(len(xs)); av=0.0
for x in xs: av+=x
av/=n
s=0.0
for x in reversed(xs): s=(x-av)**2+s
print("biased", (av, s/(n-1)))
