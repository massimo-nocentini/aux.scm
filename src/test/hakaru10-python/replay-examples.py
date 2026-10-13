import sys; sys.path.insert(0,sys.argv[1])
from engine import *
from collections import Counter
V=pure
bern=lambda p: R(Bern(p)); normal=lambda m,sd: R(Normal(m,sd)); cat=lambda l: R(Categorical(l))
def pbern(s): return dist(s,bern,V(0.4))
print("pbern",Counter(mcmC(10,pbern)))
def ph2(s):
    x=dist(s,bern,V(0.2)); return dist(s,bern,fmap(lambda xv: 0.1 if xv else 0.7,x))
print("ph2 5809?",sum(mcmC(10000,ph2)))
def ph21(s):
    x=dist(s,bern,V(0.2)); dist(s,bern,fmap(lambda xv: 0.1 if xv else 0.7,x)); return x
print("ph21 2026?",sum(mcmC(10000,ph21)))
def pnorm(s): return dist(s,normal,V(10.0),V(0.5))
def pdep1(s): x=pnorm(s); return diracN(s,lift2(lambda a,b:a+b,x,V(1.0)))
a=mcmC(100,lambda s: dist(s,normal,V(11.0),V(0.5))); b=mcmC(100,pdep1)
print("pdep_run", a==b, max(abs(x-y) for x,y in zip(a,b)))
def pcond1(s):
    x=dist(s,cat,V([(1,0.5),(2,0.5)]))
    return if_(s,fmap(lambda v:v==1,x),lambda s: dist(s,cat,V([(10,0.5),(11,0.5)])),lambda s: dist(s,cat,V([(20,0.5),(21,0.5)])))
print("pcond1 (2481,2394,2565,2560)?",sorted(Counter(mcmC(10000,pcond1)).items()))
def pcond2(s):
    x=dist(s,cat,V([(1,0.5),(2,0.5)]))
    return if_(s,fmap(lambda v:v==1,x),lambda s: x,lambda s: dist(s,cat,V([(20,0.5),(21,0.5)])))
print("pcond2 (5000,2321,2679)?",sorted(Counter(mcmC(10000,pcond2)).items()))
def pcond3(s):
    x=dist(s,cat,V([(1,0.5),(2,0.5)]))
    def el(s):
        dist(s,normal,V(0.0),V(1.0)); return dist(s,cat,V([(20,0.5),(21,0.5)]))
    return if_(s,fmap(lambda v:v==1,x),lambda s: x,el)
print("pcond3 (5040,2509,2451)?",sorted(Counter(mcmC(10000,pcond3)).items()))
def pcond4(s):
    x=dist(s,cat,V([(1,0.5),(2,0.5)]))
    def el(s):
        dist(s,condition(True,bern),V(0.5)); return dist(s,cat,V([(20,0.5),(21,0.5)]))
    return if_(s,fmap(lambda v:v==1,x),lambda s: x,el)
print("pcond4 1000 (648,174,178)?",sorted(Counter(mcmC(1000,pcond4)).items()))
print("pcond4 10000 (6697,1641,1662)?",sorted(Counter(mcmC(10000,pcond4)).items()))
def fuzzy2(pf,op):
    return lambda x,y: cat([(op(x,y),pf),(not op(x,y),1-pf)])
def pcorr(s):
    x=dist(s,bern,V(0.5)); y=dist(s,bern,V(0.5))
    dist(s,condition(True,fuzzy2(0.1,lambda a,b: not (b==a))),x,y); return y
print("pcorr (419F,581T)?",Counter(mcmC(1000,pcorr)))
def girl(s):
    g1=dist(s,bern,V(0.5)); g2=dist(s,bern,V(0.5))
    dist(s,condition(True,lambda x,y: cat([(x or y,0.99),(not (x or y),0.01)])),g1,g2); return g1
print("girl 77?",sum(mcmC(100,girl)))
def post_bias(s):
    coin=dist(s,bern,V(0.5))
    if_(s,coin,lambda s: dist(s,condition(1.0,normal),V(0.0),V(1.0)),lambda s: dist(s,condition(1.0,normal),V(100.0),V(1.0)))
    return coin
print("post_bias",mcmC(10,post_bias))
def nnot(p,b): return p if b else 1
def nor(sx,sy,noise): return lambda x,y: bern(1-nnot(1-sx,x)*nnot(1-sy,y)*(1-noise))
def grass(s):
    rain=dist(s,bern,V(0.3)); spr=dist(s,bern,V(0.5)); dist(s,condition(True,nor(0.9,0.8,0.1)),rain,spr); return rain
r=mcmC(20000,grass); print("grass 0.46815 (old)?", sum(r)/20000.0)
