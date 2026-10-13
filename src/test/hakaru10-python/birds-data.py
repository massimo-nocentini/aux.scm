import math, random, sys
random.seed(17)
out=sys.argv[1]
Y,D=30,20
beta=[2.1,4.5,3.0,3.7]
def xy(n): m=n-1; return (m//4, m%4)
def feats(y,d,i,j,wind):
    if i==j: return [0.0,0.0,0.0,2.0]
    p,q=xy(i),xy(j); v=(q[0]-p[0],q[1]-p[1]); dd=math.hypot(*v)
    f1=4*math.exp(-(math.log(dd)-1)**2/2)/(dd*math.sqrt(2*math.pi))
    u=(3-p[0],3-p[1]); nu=math.hypot(*u)
    f2=0.0 if nu==0 else (v[0]*u[0]+v[1]*u[1])/(dd*nu)
    f3=(v[0]*wind[0]+v[1]*wind[1])/dd
    return [f1,f2,f3,0.0]
F={}
with open(out+'/onebird_features.csv','w') as f:
    f.write('Year,Day,FromCell,ToCell,f1,f2,f3,f4\r\n')
    for y in range(1,Y+1):
        for d in range(1,D):
            a=random.uniform(0,2*math.pi); w=(math.cos(a),math.sin(a))
            for i in range(1,17):
                for j in range(1,17):
                    fs=feats(y,d,i,j,w); F[(y,d,i,j)]=fs
                    f.write('%d,%d,%d,%d,%s\r\n'%(y,d,i,j,','.join('%.6f'%x for x in fs)))
def poisson(l):
    L=math.exp(-l);k=0;p=1.0
    while True:
        p*=random.random()
        if p<=L: return k
        k+=1
with open(out+'/onebird-observations.csv','w') as f:
    f.write('Year,Day,Cell1,Cell2,Cell3,Cell4,Cell5,Cell6,Cell7,Cell8,Cell9,Cell10,Cell11,Cell12,Cell13,Cell14,Cell15,Cell16\r\n')
    for y in range(1,Y+1):
        n=1
        for d in range(1,D+1):
            counts=[poisson(1.0) if c==n else 0 for c in range(1,17)]
            f.write('%d,%d,%s\r\n'%(y,d,','.join(map(str,counts))))
            if d<D:
                ws=[math.exp(sum(a*b for a,b in zip(F[(y,d,n,j)],beta))) for j in range(1,17)]
                r=random.random()*sum(ws); acc=0
                for j,wj in enumerate(ws,1):
                    acc+=wj
                    if r<acc: n=j; break
