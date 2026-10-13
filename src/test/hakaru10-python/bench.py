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
print(momenta(mcmC(int(sys.argv[2]) if len(sys.argv)>2 else 10000,phier(10))))
