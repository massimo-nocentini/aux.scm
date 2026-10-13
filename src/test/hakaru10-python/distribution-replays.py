import sys, math
def fmt(v):
    if isinstance(v,list): return '('+' '.join(fmt(y) for y in v)+')'
    if isinstance(v,bool): return '#t' if v else '#f'
    if isinstance(v,str): return v
    if isinstance(v,int): return str(v)
    return '%.17g' % v
sys.path.insert(0, sys.argv[1])
from engine import *
from extra import *
def poisson(lam, g):
    expl=math.exp(-lam)
    if lam==0: return 0,g
    u,g=rR(0.0,1.0,g); p=u; k=1
    while p>expl:
        u,g=rR(0.0,1.0,g); p=p*u; k+=1
    return k-1,g
def gamma_fixed(shape,scl,g):
    if shape<1.0:
        v,g=gamma_rng(shape+1,1.0,g); w,g=rR(0.0,1.0,g); return scl*v*(w**(1/shape)),g
    return gamma_rng(shape,scl,g)
def laplace(mu,sd,g):
    u,g=rR(0.0,1.0,g)
    return (mu+sd*math.log(u+u) if u<0.5 else mu-sd*math.log(2.0-u-u)),g
def dirichlet(n,a,g):
    xs=[]; tot=0.0
    for i in range(n):
        x,g=gamma_rng(a,1.0,g); xs.insert(0,x); tot=x+tot
    return [x/tot for x in xs],g
samplers=[
 ('bern', lambda g: Bern(0.3).sample(g)[0::2]),
 ('uniformly', lambda g: Uniformly(['a','b','c','d','e']).sample(g)[0::2]),
 ('categorical', lambda g: Categorical([('a',1.0),('b',3.0),('c',2.0)]).sample(g)[0::2]),
 ('uniform', lambda g: Uniform(10.0,20.0).sample(g)[0::2]),
 ('normal', lambda g: Normal(10.0,0.5).sample(g)[0::2]),
 ('gamma', lambda g: gamma_rng(3.0,1/3,g)),
 ('gamma2', lambda g: gamma_rng(2.5,2.0,g)),
 ('gammasmall', lambda g: gamma_fixed(0.5,2.0,g)),
 ('beta', lambda g: beta_rng(2.0,5.0,g)),
 ('betaj', lambda g: beta_rng(0.5,0.5,g)),
 ('betamix', lambda g: beta_rng(0.5,2.0,g)),
 ('poisson', lambda g: poisson(3.5,g)),
 ('laplace', lambda g: laplace(1.0,2.0,g)),
 ('dirichlet', lambda g: dirichlet(3,2.0,g)),
]
for name,f in samplers:
    g=mk(17); out=[]
    for i in range(200):
        x,g=f(g); out.append(x)
    print(name, ' '.join(fmt(v) for v in out), g[0], g[1])
