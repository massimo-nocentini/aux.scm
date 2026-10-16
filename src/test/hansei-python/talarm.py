import os, sys; sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from hansei_py import *
def talarm_lr(d,f):
    e=flip(d,0.01); b=flip(d,0.1)
    if e: return flip(d,0.99) if b else flip(d,0.2)
    return flip(d,0.98) if b else flip(d,0.01)
def talarm_rl(d,f):
    b=flip(d,0.1); e=flip(d,0.01)
    if e: return flip(d,0.99) if b else flip(d,0.2)
    return flip(d,0.98) if b else flip(d,0.01)
S=lambda s: random_selector(s, xorvariant=True, ndraw=3)
for m in (talarm_lr, talarm_rl):
    print(m.__name__, sample_rejection(S(1),100,m), sample_importance(S(1),100,m))
