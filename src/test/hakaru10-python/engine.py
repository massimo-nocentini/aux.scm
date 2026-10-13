import math, copy
from stdgen import *
NINF=float('-inf')
# ---- distributions (Distribution.hs)
class Bern:
    def __init__(s,p): s.p=p; s.lT=math.log(p) if p>0 else NINF; s.lF=math.log(1-p) if p<1 else NINF
    def logd(s,x): return s.lT if x else s.lF
    def sample(s,g):
        t,g=rR(0.0,1.0,g)
        return (True,s.lT,g) if t<=s.p else (False,s.lF,g)
class Categorical:
    def __init__(s,lst):
        tot=0
        for _,p in lst: tot=tot+p   # Haskell sum = foldl
        s.l=[(x,p/tot) for x,p in lst]
    def logd(s,x):
        for y,p in s.l:
            if y==x: return math.log(p)
        return NINF
    def sample(s,g):
        p,g1=rR(0.0,1.0,g)
        for x,p0 in s.l:
            if p<p0: return (x,math.log(p0),g1)
            p=p-p0
        raise Exception("categorical")
class Uniformly:
    def __init__(s,l): s.a=list(l); s.ll=-math.log(float(len(l)))
    def logd(s,x): return s.ll if x in s.a else NINF
    def sample(s,g):
        i,g=ival(0,len(s.a)-1,g); return (s.a[i],s.ll,g)
def marsaglia(g):
    while True:
        x,g1=rR(-1.0,1.0,g); y,g2=rR(-1.0,1.0,g1)
        s=x*x+y*y
        if 1>=s and s>0:
            q=math.sqrt((-2)*math.log(s)/s); return (x*q,y*q),g2
        g=g2
class Normal:
    def __init__(s,mu,sd): s.mu=mu; s.sd=sd; s.tau=1/(sd*sd); s.lf=math.log(s.tau/math.pi/2)
    def logd(s,x): return (-s.tau*((x-s.mu)*(x-s.mu))+s.lf)/2
    def sample(s,g):
        (z,_),g1=marsaglia(g); x=s.mu+s.sd*z; return (x,s.logd(x),g1)
def resample(d,old,llold,g):
    v,ll,g=d.sample(g); return (v,ll,g),ll-llold
# ---- engine (Metropolis.hs)
class Node:
    __slots__=('val','ll','dst','kind','tstamp','update')
    def __init__(s,**k):
        for a,b in k.items(): setattr(s,a,b)
    def copy(s,**k):
        n=Node(val=s.val,ll=s.ll,dst=s.dst,kind=s.kind,tstamp=s.tstamp,update=s.update)
        for a,b in k.items(): setattr(n,a,b)
        return n
class St: pass
def init_state(g):
    s=St(); s.seed=g; s.now=1; s.nodes={}; s.count=0; s.deps={}; s.inactive={}; s.delayed=set()
    s.llcum=0.0; s.sampleable=[]; s.added=set(); s.card=0; return s
def sampleK(dk,g):
    if dk[0]=='R': return dk[1].sample(g)
    if dk[0]=='D': return (dk[1],0.0,g)
    return (dk[2],dk[1].logd(dk[2]),g)
def create_node(s,upd,dk):
    v,ll,g=sampleK(dk,s.seed); s.seed=g
    s.count+=1; a=s.count
    s.nodes[a]=Node(val=v,ll=ll,dst=(dk[1] if dk[0]!='D' else None),kind=dk[0],tstamp=s.now,update=upd)
    if dk[0]=='R': s.sampleable.insert(0,a)
    s.added.add(a); s.llcum=s.llcum+ll
    return a
def new_dep(s,p,c): s.deps.setdefault(p,set()).add(c)
def modify_node(s,a,n):
    old=s.nodes[a]; s.nodes[a]=n; s.llcum=s.llcum+n.ll-old.ll
    return set() if old.tstamp==n.tstamp else set(s.deps.get(a,()))
def propagate(s,q):
    q=set(q)
    while q:
        a=min(q); q.discard(a)
        if a in s.inactive: s.delayed.add(a); continue
        n=s.nodes[a]
        if n.tstamp>=s.now: raise Exception("updating the already updated")
        if n.update is None: continue
        n2,extra=n.update(s,a)
        q|=extra|modify_node(s,a,n2)
# SExp: ('V',v) ('N',a) ('T',tree,f); tree ('L',a)|('P',t1,t2)
def ev_tree(s,t):
    if t[0]=='L': n=s.nodes[t[1]]; return n.val,n.tstamp
    v1,t1=ev_tree(s,t[1]); v2,t2=ev_tree(s,t[2]); return (v1,v2),max(t1,t2)
def leaves(t): return [t[1]] if t[0]=='L' else leaves(t[1])+leaves(t[2])
def fmap(f,e):
    if e[0]=='V': return ('V',f(e[1]))
    if e[0]=='N': return ('T',('L',e[1]),f)
    g=e[2]; return ('T',e[1],lambda x: f(g(x)))
def ap(ef,ex):
    if ef[0]=='V' and ex[0]=='V': return ('V',ef[1](ex[1]))
    if ef[0]=='V' and ex[0]=='N': return ('T',('L',ex[1]),ef[1])
    if ef[0]=='T' and ex[0]=='V': g=ef[2]; y=ex[1]; return ('T',ef[1],lambda ab: g(ab)(y))
    if ef[0]=='V' and ex[0]=='T': f=ef[1]; g=ex[2]; return ('T',ex[1],lambda ab: f(g(ab)))
    if ef[0]=='T' and ex[0]=='N': g=ef[2]; return ('T',('P',ef[1],('L',ex[1])),lambda p: g(p[0])(p[1]))
    if ef[0]=='T' and ex[0]=='T': g1=ef[2]; g2=ex[2]; return ('T',('P',ef[1],ex[1]),lambda p: g1(p[0])(g2(p[1])))
    raise Exception("cannot happen")
def lift2(f,a,b): return ap(fmap(lambda x: lambda y: f(x,y),a),b)
def pure(v): return ('V',v)
def mkupd(tree,df):
    def upd(s,a):
        self=s.nodes[a]; tv,ts=ev_tree(s,tree)
        if ts<=self.tstamp: raise Exception("dependency violation")
        dk=df(tv)
        if dk[0]=='D': return self.copy(val=dk[1],tstamp=s.now),set()
        d=dk[1]; return self.copy(ll=d.logd(self.val),dst=d),set()
    return upd
def with_node_ctor(s,e,df):
    if e[0]=='V': return create_node(s,None,df(e[1]))
    tree=('L',e[1]) if e[0]=='N' else e[1]
    f=(lambda x:x) if e[0]=='N' else e[2]
    dff=lambda tv: df(f(tv))
    a=create_node(s,mkupd(tree,dff),dff(ev_tree(s,tree)[0]))
    for p in leaves(tree): new_dep(s,p,a)
    return a
R=lambda d: ('R',d)
def dist(s,ctor,*args):
    if len(args)==0: return ('N',create_node(s,None,ctor))
    if len(args)==1: return ('N',with_node_ctor(s,args[0],ctor))
    return ('N',with_node_ctor(s,lift2(lambda a,b:(a,b),args[0],args[1]),lambda t: ctor(*t)))
def condition(v,ctor):
    def c(*xs):
        dk=ctor(*xs); assert dk[0]=='R'; return ('O',dk[1],v)
    return c
dirac=lambda v: ('D',v)
def diracN(s,e):
    if e[0]=='V': return ('N',create_node(s,None,dirac(e[1])))
    if e[0]=='N': return e
    return ('N',with_node_ctor(s,e,dirac))
def to_node(s,e):
    if e[0]=='V': return create_node(s,None,dirac(e[1]))
    if e[0]=='N': return e[1]
    return with_node_ctor(s,e,dirac)
def if_(s,test,th,el):
    entry=with_node_ctor(s,test,dirac)
    return ('N',ifnode(s,entry,lambda: to_node(s,th(s)),lambda: to_node(s,el(s))))
def deactivate(s,ns):
    for a in ns:
        if a in s.inactive: s.inactive[a]+=1
        else:
            s.inactive[a]=1; n=s.nodes[a]
            if n.kind=='O': s.llcum=s.llcum-n.ll  # Haskell: llcum - sum(llc) ; order differs, see note
            if n.kind=='R': s.card-=1
def deactivate_h(s,ns):
    llc=0.0; k=0
    for a in sorted(ns):
        if a in s.inactive: s.inactive[a]+=1
        else:
            s.inactive[a]=1; n=s.nodes[a]
            llc=llc+(n.ll if n.kind=='O' else 0)
            if n.kind=='R': k+=1
    s.llcum=s.llcum-llc; s.card-=k
def activate_h(s,ns):
    llc=0.0; k=0
    for a in sorted(ns):
        c=s.inactive[a]
        if c==1:
            del s.inactive[a]; n=s.nodes[a]
            llc=llc+(n.ll if n.kind=='O' else 0)
            if n.kind=='R': k+=1
        else: s.inactive[a]=c-1
    s.llcum=s.llcum+llc; s.card+=k
def ifnode(s,entry,th,el):
    if entry in s.sampleable: raise Exception("entry sampleable")
    tv=s.nodes[entry].val
    act,ina=(th,el) if tv else (el,th)
    a0=set(s.added); ina_nr=ina(); a1=set(s.added); ina_nodes=a1-a0
    deactivate_h(s,ina_nodes)
    act_nr=act(); a2=set(s.added); act_nodes=a2-a1
    th_nr,el_nr=(act_nr,ina_nr) if tv else (ina_nr,act_nr)
    def upd_exit(s,a):
        self=s.nodes[a]; en=s.nodes[entry]; r=th_nr if en.val else el_nr; nd=s.nodes[r]
        if en.tstamp<=self.tstamp and nd.tstamp<=self.tstamp: raise Exception("exit parents")
        return self.copy(val=nd.val,tstamp=s.now),set()
    ex=create_node(s,upd_exit,dirac(s.nodes[act_nr].val))
    new_dep(s,entry,ex); new_dep(s,act_nr,ex); new_dep(s,ina_nr,ex)
    ths,els=(act_nodes,ina_nodes) if tv else (ina_nodes,act_nodes)
    prev=s.nodes[entry].update
    def upd_entry(s,a):
        old=s.nodes[a].val
        n2,deps=prev(s,a)
        if n2.val==old: return n2,deps
        na,ni=(ths,els) if n2.val else (els,ths)
        deactivate_h(s,ni); activate_h(s,na)
        nd=na & s.delayed; s.delayed-=nd
        return n2,deps|nd
    n=s.nodes[entry]; s.nodes[entry]=n.copy(update=upd_entry)
    return ex
def mcmC(limit,model,seed=17):
    g=mk(seed)
    def build(g):
        s=init_state(g); r=to_node(s,model(s)); return r,s
    lim=limit; r,s=build(g)
    while lim>0 and math.isinf(s.llcum):
        lim-=1; r,s=build(s.seed)
    if lim==0: return []
    out=[]
    for _ in range(limit):
        out.append(s.nodes[r].val)
        s2=copy.copy(s); s2.nodes=dict(s.nodes); s2.inactive=dict(s.inactive); s2.delayed=set(s.delayed)
        s2.now+=1; s2.llcum=0.0
        cold=s2.card; N=len(s2.sampleable)
        while True:
            i,s2.seed=ival(0,N-1,s2.seed); a=s2.sampleable[i]
            if a not in s2.inactive: break
        n=s2.nodes[a]
        (v,llv,g2),fb=resample(n.dst,n.val,n.ll,s2.seed); s2.seed=g2
        q=modify_node(s2,a,n.copy(val=v,ll=llv,tstamp=s2.now))
        propagate(s2,q)
        ld=s2.llcum; cn=s2.card
        if math.isinf(ld): acc=False
        else:
            ratio=ld-fb+(0 if cn==cold else math.log(float(N+cold)/float(N+cn)))
            if ratio>=0: acc=True
            else:
                u,s2.seed=rR(0.0,1.0,s2.seed); acc=u<math.exp(ratio)
        if acc: s=s2
        else: s.seed=s2.seed
    return out
