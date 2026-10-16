import os, sys; sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from ocamlrandom import R
def sel(rng, ps):
    tot=0.0
    for p in ps: tot+=p
    r=rng.float(tot); c=0.0
    for i,p in enumerate(ps):
        c+=p
        if r<c: return i
def inner(rng):  # sample_rejection rs 10 (fun () -> flip 0.5)
    t=0
    for _ in range(10):
        if sel(rng,[0.5,0.5])==0: t+=1
    return (t/10.0)
def run(globalmode):
    g=R(17,True,3)
    counts={}
    for _ in range(10):
        b=sel(g,[0.5,0.5])
        if globalmode: ir=R(17,True,3); g=ir   # Random.init 17 resets the shared global
        else: ir=R(17,True,3)
        v = 'biased' if b==1 else inner(ir)
        counts[v]=counts.get(v,0)+1
    return counts
print("exact-outer inner table:", inner(R(17,True,3)))
print("global (upstream):", run(True))
print("fresh  (plan dev18):", run(False))
