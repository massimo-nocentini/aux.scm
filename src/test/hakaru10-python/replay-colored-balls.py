import sys; sys.path.insert(0,sys.argv[1])
import engine, time
from extra import *
from collections import Counter
V=pure
flipmode=sys.argv[2]=='flip'
orig=engine.resample
def flip(d,old,llold,g):
    if isinstance(d,Bern): x=not old; return (x,d.logd(x),g),0.0
    return orig(d,old,llold,g)
if flipmode: engine.resample=flip
bern=lambda p: R(Bern(p)); cat=lambda l: R(Categorical(l)); unif=lambda l: R(Uniformly(l))
def ph21(s):
    x=dist(s,bern,V(0.2)); dist(s,bern,fmap(lambda xv: 0.1 if xv else 0.7,x)); return x
print(sys.argv[2],"ph21 2026?",sum(mcmC(10000,ph21)))
def girl(s):
    g1=dist(s,bern,V(0.5)); g2=dist(s,bern,V(0.5))
    dist(s,condition(True,lambda x,y: cat([(x or y,0.99),(not (x or y),0.01)])),g1,g2); return g1
print(sys.argv[2],"girl 77?",sum(mcmC(100,girl)))
if not flipmode:
    opp=lambda c: 'G' if c=='B' else 'B'
    oc=lambda c: cat([(c,0.8),(opp(c),0.2)])
    def cballs(obs):
        def m(s):
            balls={i+1: dist(s,unif,V(['B','G'])) for i in range(8)}
            nballs=dist(s,unif,V(list(range(1,9))))
            def balls_observe(n):
                def bo(s):
                    for color in obs:
                        b=dist(s,unif,V(list(range(1,n+1))))
                        def chain(i):
                            if i>8: return lambda s: V(color)
                            return lambda s: if_(s,fmap(lambda v,i=i: v==i,b),lambda s,i=i: dist(s,condition(color,oc),balls[i]),chain(i+1))
                        chain(1)(s)
                    return V(())
                return bo
            def outer(i):
                if i>8: return lambda s: V(())
                return lambda s: if_(s,fmap(lambda v,i=i: v==i,nballs),balls_observe(i),outer(i+1))
            outer(1)(s)
            return nballs
        return m
    t=time.time(); r=mcmC(10000,cballs(['B']*10))
    print("cbr {1:4265,2:2203,3:1244,4:805,5:486,6:291,7:565,8:141}?",sorted(Counter(r).items()),time.time()-t)
