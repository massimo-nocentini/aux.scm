import hashlib, sys
class R:
    def __init__(s, seed, xorvariant=True, ndraw=2):
        s.xv=xorvariant; s.nd=ndraw
        seeds=[seed]; l=1
        s.st=list(range(55)); accu=b"x"
        for i in range(0, 55+max(55,l)):
            j=i%55; k=i%l
            accu=hashlib.md5(accu+str(seeds[k]).encode()).digest()
            ex=accu[0]|(accu[1]<<8)|(accu[2]<<16)|(accu[3]<<24)
            s.st[j]=(s.st[j]^ex)&0x3FFFFFFF
        s.idx=0
    def bits(s):
        s.idx=(s.idx+1)%55
        cur=s.st[s.idx]
        if s.xv: new=s.st[(s.idx+24)%55]+(cur^((cur>>25)&0x1F))
        else: new=s.st[(s.idx+24)%55]+cur
        new&=0x3FFFFFFF; s.st[s.idx]=new; return new
    def float(s,b):
        sc=1073741824.0
        if s.nd==2:
            r1=float(s.bits()); r2=float(s.bits()); return ((r1/sc+r2)/sc)*b
        r0=float(s.bits()); r1=float(s.bits()); r2=float(s.bits())
        return (((r0/sc+r1)/sc+r2)/sc)*b
def flip2(rng,n):
    t=0
    for _ in range(n):
        if rng.float(1.0) < 0.5: t+=1
    return t
def geom(rng,n,p=0.85):
    from collections import Counter
    c=Counter()
    for _ in range(n):
        k=0
        while True:
            r=rng.float(p+(1.0-p))
            if r < p: break
            k+=1
        c[k]+=1
    return dict(c)
if __name__=='__main__':
 for xv in (True,False):
  for nd in (2,3):
   print(xv,nd,"flip2 seed1:",flip2(R(1,xv,nd),100),"(expect 48)", "noshared seed1:",
          sum(1 for _ in range(1)) , "geom seed17:", geom(R(17,xv,nd),100), "(expect {0:79,1:17,2:4})")
