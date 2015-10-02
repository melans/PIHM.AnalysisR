
spihm()
Dinf=0.1

ks= 37.9    #m/day
km=606.6    #m/day
alpha=6.58 * 1.5
beta=1.238 *1.15
af=0.01 * 10
sd=1.4
z=sd
Yu=seq(0.1, z-Dinf, length.out=1000)

y0=0.001
fn=paste('KV_','sd',sd, 'af',af, 'y0',y0,'z',z,'.jpg', sep='')
ke=KVs(ks=ks, km=km, alpha=alpha, beta=beta, af=af,Ysurf=y0, De=z, Yu=Yu, if.save=TRUE,fn=fn)



y0=0.01
fn=paste('KV_','sd',sd, 'af',af, 'y0',y0,'z',z,'.jpg', sep='')
ke=KVs(ks=ks, km=km, alpha=alpha, beta=beta, af=af,Ysurf=y0, De=z, Yu=Yu, if.save=TRUE,fn=fn)


y0=0.1
fn=paste('KV_','sd',sd, 'af',af, 'y0',y0,'z',z,'.jpg', sep='')
ke=KVs(ks=ks, km=km, alpha=alpha, beta=beta, af=af,Ysurf=y0, De=z, Yu=Yu, if.save=TRUE,fn=fn)


y0=1
fn=paste('KV_','sd',sd, 'af',af, 'y0',y0,'z',z,'.jpg', sep='')
ke=KVs(ks=ks, km=km, alpha=alpha, beta=beta, af=af,Ysurf=y0, De=z, Yu=Yu, if.save=TRUE,fn=fn)


spihm()
Dinf=0.1

ks= 5    #m/day
km= 500   #m/day
alpha=6.58
beta= 1.538
af= 0.01 * 10
sd=20
z=sd
Yu=seq(0.1, z-Dinf, length.out=1000)


y0=0.001
fn=paste('KV_','sd',sd, 'af',af, 'y0',y0,'z',z,'.jpg', sep='')
ke=KVs(ks=ks, km=km, alpha=alpha, beta=beta, af=af,Ysurf=y0, De=z, Yu=Yu, if.save=TRUE,fn=fn)



y0=0.01
fn=paste('KV_','sd',sd, 'af',af, 'y0',y0,'z',z,'.jpg', sep='')
ke=KVs(ks=ks, km=km, alpha=alpha, beta=beta, af=af,Ysurf=y0, De=z, Yu=Yu, if.save=TRUE,fn=fn)


y0=0.1
fn=paste('KV_','sd',sd, 'af',af, 'y0',y0,'z',z,'.jpg', sep='')
ke=KVs(ks=ks, km=km, alpha=alpha, beta=beta, af=af,Ysurf=y0, De=z, Yu=Yu, if.save=TRUE,fn=fn)


y0=1
fn=paste('KV_','sd',sd, 'af',af, 'y0',y0,'z',z,'.jpg', sep='')
ke=KVs(ks=ks, km=km, alpha=alpha, beta=beta, af=af,Ysurf=y0, De=z, Yu=Yu, if.save=TRUE,fn=fn)

