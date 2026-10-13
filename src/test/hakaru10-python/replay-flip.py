import sys; sys.path.insert(0,sys.argv[1])
import engine
from extra import *
V=pure
orig=engine.resample
def flip(d,old,llold,g):
    if isinstance(d,Bern): x=not old; return (x,d.logd(x),g),0.0
    return orig(d,old,llold,g)
engine.resample=flip
bern=lambda p: R(Bern(p)); normal=lambda m,sd: R(Normal(m,sd)); uniform=lambda a,b: R(Uniform(a,b)); gamma=lambda a,b: R(Gamma(a,b))
def mixng(s):
    x=dist(s,normal,V(0.0),V(1.0)); return if_(s,fmap(lambda v:v>0,x),lambda s: dist(s,normal,V(10.0),V(2.0)),lambda s: dist(s,gamma,V(3.0),V(1/3)))
def mixng2(s):
    x=dist(s,bern,V(0.5)); return if_(s,x,lambda s: dist(s,normal,V(10.0),V(2.0)),lambda s: dist(s,gamma,V(3.0),V(1/3)))
print("flip mixng KL 1.13e-2 / 2.2758e-2?",kl(tabulate(0.5,mcmC(5000,mixng)),tabulate(0.5,mcmC(5000,mixng2))))
def tac1(s):
    c=dist(s,bern,V(0.5)); if_(s,c,lambda s: dist(s,normal,V(0.0),V(1.0)),lambda s: dist(s,uniform,V(10.0),V(20.0))); return c
print("flip tac1 620?",sum(mcmC(1000,tac1)))
def ph2(s):
    x=dist(s,bern,V(0.2)); return dist(s,bern,fmap(lambda xv: 0.1 if xv else 0.7,x))
print("flip ph2 5809?",sum(mcmC(10000,ph2)))
print("flip pbern", sum(mcmC(10,lambda s: dist(s,bern,V(0.4)))))
