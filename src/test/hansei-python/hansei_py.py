import os, sys; sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))
from ocamlrandom import R
NEARLY_ONE = 1.0 - 1e-7
class Fail(Exception): pass
class Need(Exception):
    def __init__(s,ch): s.ch=ch
def reify0(model):
    def node(prefix):
        it=iter(prefix)
        def dist(ch):
            try: i=next(it)
            except StopIteration: raise Need(ch)
            return ch[i][1]
        def fail(): raise Fail()
        try: v=model(dist,fail)
        except Fail: return []
        except Need as n:
            return [(p,('C',(lambda i=i: node(prefix+[i])))) for i,(p,_) in enumerate(n.ch)]
        return [(1.0,('V',v))]
    return node([])
# PMap-like: dict preserving insertion; final output sorted desc by key (OCaml compare)
def ins(d,v,p):
    d[v]= p + d[v] if v in d else p
def out(d, scale=None):
    ks=sorted(d.keys(), reverse=True)
    return [((d[k]/scale) if scale else d[k], ('V',k)) for k in ks]
def random_selector(seed, **kw):
    rng=R(seed, **kw)
    def sel(choices):
        ptotal=0.0
        for p,_ in choices: ptotal=ptotal+p
        r=rng.float(ptotal); pcum=0.0
        for p,th in choices:
            pcum=pcum+p
            if r<pcum: return (ptotal,th)
        raise Exception("can't happen")
    return sel
def explore(maxdepth, choices):
    ans={}; susp=[]
    def loop(p,depth,down,choices):
        for pt,(tag,x) in choices:
            if tag=='V': ins(ans,x,pt*p)
            elif down:
                loop(pt*p, depth+1, (maxdepth is None or depth<maxdepth), x())
            else: susp.insert(0,(pt*p,(tag,x)))
    loop(1.0,0,True,choices)
    return out(ans)+susp
def shallow_explore(maxdepth, choices):
    ans={}
    def loop(pc,depth,acc,lst):
        for item in lst:
            p,(tag,x)=item
            if tag=='V': ins(ans,x,p*pc)
            elif depth>=maxdepth: acc=[item]+acc
            else:
                ch=loop(pc*p,depth+1,[],x())
                ptotal=0.0
                for q,_ in ch: ptotal=ptotal+q   # fold_left (fun pa (p,_) -> pa +. p)
                if ptotal==0.0: pass
                elif ptotal<NEARLY_ONE:
                    ch2=[(q/ptotal,y) for q,y in ch]
                    acc=[(p*ptotal,('C',(lambda c=ch2: c)))]+acc
                else: acc=[(p,('C',(lambda c=ch: c)))]+acc
        return acc
    susp=loop(1.0,0,[],choices)
    return out(ans)+susp
def rejection_sample_dist(selector,nsamples,ch):
    ans={}
    def loop(pcontrib,ch):
        while True:
            if len(ch)==1 and ch[0][1][0]=='V': ins(ans,ch[0][1][1],ch[0][0]*pcontrib); return
            if not ch: return
            if len(ch)==1: pcontrib=ch[0][0]*pcontrib; ch=ch[0][1][1](); continue
            ptotal,th=selector(ch); pcontrib=pcontrib*ptotal; ch=[(1.0,th)]
    for _ in range(nsamples): loop(1.0,ch)
    return out(ans, float(nsamples))
def sample_dist(selector, runner, ch):
    def look_ahead(pcontrib, ans, acc, item):
        p,(tag,x)=item
        if tag=='V': ins(ans,x,p*pcontrib); return acc
        c=x()
        if not c: return acc
        if len(c)==1 and c[0][1][0]=='V': ins(ans,c[0][1][1],p*c[0][0]*pcontrib); return acc
        ptotal=0.0
        for q,_ in c: ptotal=ptotal+q
        if ptotal<NEARLY_ONE: return [(p*ptotal,[(q/ptotal,y) for q,y in c])]+acc
        return [(p,c)]+acc
    def loop(pcontrib, ans, ch):
        while True:
            if len(ch)==1 and ch[0][1][0]=='V': ins(ans,ch[0][1][1],ch[0][0]*pcontrib); return ans
            if not ch: return ans
            if len(ch)==1: pcontrib=ch[0][0]*pcontrib; ch=ch[0][1][1](); continue
            acc=[]
            for it in ch: acc=look_ahead(pcontrib,ans,acc,it)
            if not acc: return ans
            ptotal,th=selector(acc); pcontrib=pcontrib*ptotal; ch=th
    def toploop(pcontrib, ans, cch):
        ptotal,th=selector(cch); return loop(pcontrib*ptotal, ans, th)
    def driver(pcontrib, vals, cch):
        ans,ns=runner({}, lambda a: toploop(pcontrib,a,cch))
        ns=float(ns)
        for v in sorted(vals.keys()):   # PMap.foldi ascending order
            ins(ans,v,ns*vals[v])
        return out(ans, ns)
    def make_threads(pcontrib, ans, ch):
        while True:
            acc=[]
            for it in ch: acc=look_ahead(pcontrib,ans,acc,it)
            if not acc: return out(ans)
            if len(acc)==1: pcontrib=pcontrib*acc[0][0]; ch=acc[0][1]; continue
            return driver(pcontrib, ans, list(reversed(acc)))
    return make_threads(1.0,{},ch)
def runner_n(n):
    def r(z,th):
        for _ in range(n): z=th(z)
        return (z,n)
    return r
def sample_rejection(sel,n,model): return rejection_sample_dist(sel,n,reify0(model))
def sample_importance(sel,n,model): return sample_dist(sel,runner_n(n),shallow_explore(3,reify0(model)))
def exact_reify(model): return explore(None, reify0(model))
def flip(dist,p): return dist([(p,True),(1.0-p,False)])
