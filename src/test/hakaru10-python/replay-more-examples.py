import sys; sys.path.insert(0,sys.argv[1])
from extra import *
from collections import Counter
V=pure
bern=lambda p: R(Bern(p)); normal=lambda m,sd: R(Normal(m,sd)); uniform=lambda a,b: R(Uniform(a,b))
beta=lambda a,b: R(Beta(a,b)); gamma=lambda a,b: R(Gamma(a,b)); unif=lambda l: R(Uniformly(l)); cat=lambda l: R(Categorical(l))
def phier(n):
    def m(s):
        x=dist(s,normal,V(0.0),V(1.0))
        for _ in range(n): x=dist(s,normal,x,V(3.0))
        return x
    return m
print("bench 1e3 (3.4117556376053577,32.27027981263249)?",momenta(mcmC(1000,phier(10))))
print("bench 1e4 (-3.151396208304257,54.17557680794696)?",momenta(mcmC(10000,phier(10))))
def bc(c1,c2,dn=False):
    def m(s):
        b=dist(s,beta,V(1.0),V(1.0)); dist(s,condition(c1,bern),b); dist(s,condition(c2,bern),b)
        return diracN(s,b) if dn else b
    return m
print("biased_coin22 (0.49910027237934973,0.0497358737659099)?",momenta(mcmC(10000,bc(True,False))[1000:]))
print("niceexmcr11 499.799988743311?",sum(mcmC(1000,bc(True,False,True))))
print("niceexmcr12 257.2928205920028?",sum(mcmC(1000,bc(False,False,True))))
def tac(k):
    def m(s):
        c=dist(s,bern,V(0.5))
        def th(s):
            if k>=2: diracN(s,V(True))
            return dist(s,normal,V(0.0),V(1.0))
        def el(s):
            u=dist(s,uniform,V(10.0),V(20.0))
            return diracN(s,u) if k==3 else u
        if_(s,c,th,el); return c
    return m
for k,e in ((1,620),(2,523),(3,532)): print("tac%d %d?"%(k,e),sum(mcmC(1000,tac(k))))
def tac10(s):
    c=dist(s,bern,V(0.5)); if_(s,c,lambda s: dist(s,condition(0.0,normal),V(0.0),V(1.0)),lambda s: dist(s,condition(0.0,uniform),V(10.0),V(20.0))); return c
print("tac10 100?",sum(mcmC(100,tac10)))
def pm(s):
    c=dist(s,bern,V(0.5)); if_(s,c,lambda s: dist(s,condition(-2.0,normal),V(1.0),V(1.0)),lambda s: dist(s,condition(-2.0,uniform),V(0.0),V(3.0))); return c
print("prog_mixture1 all true?",mcmC(20,pm))
def niceex1(s):
    c=dist(s,uniform,V(0.0),V(1.0)); d=dist(s,bern,V(0.5)); e=dist(s,bern,c); return diracN(s,lift2(lambda a,b:a and b,e,d))
print("niceex1 all True?",mcmC(7,niceex1))
def mixng(s):
    x=dist(s,normal,V(0.0),V(1.0)); return if_(s,fmap(lambda v:v>0,x),lambda s: dist(s,normal,V(10.0),V(2.0)),lambda s: dist(s,gamma,V(3.0),V(1/3)))
def mixng2(s):
    x=dist(s,bern,V(0.5)); return if_(s,x,lambda s: dist(s,normal,V(10.0),V(2.0)),lambda s: dist(s,gamma,V(3.0),V(1/3)))
n=5000
print("mixng KL 1.1301558326703463e-2?",kl(tabulate(0.5,mcmC(n,mixng)),tabulate(0.5,mcmC(n,mixng2))))
try:
    print("mixng vs interleave 1.3896442111895212e-2?",kl(tabulate(0.5,mcmC(n,mixng)),tabulate(0.5,mcmC(n//2,lambda s: dist(s,normal,V(10.0),V(2.0)))+mcmC(n//2,lambda s: dist(s,gamma,V(3.0),V(1/3))))))
except Exception as e: print("interleave err",e)
for n,e in ((1000,0.7862745014080637),(10000,0.23618257021796316)):
    try: print("phier_kl",n,e,"?",kl(tabulate(0.5,mcmC(n,phier(10))),tabulate(0.5,mcmC(n,lambda s: dist(s,normal,V(0.0),V(math.sqrt(91)))))))
    except Exception as ex: print("phier_kl err",ex)
