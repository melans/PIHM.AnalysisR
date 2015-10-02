#############################################################
# Notes
# Assumption is to specify wdir location of scripts
#
# To start Lele's Tool
# source("EffectiveKV_Alone.R")
#
# See Notes for packages needed
#############################################################

## NEED THE FOLLOWING R FILES

source("pihm.plot.R")
source("KV_effective.R")

#############################################################

## USER NEEDS TO SPECIFY FOLLOWING VARIABLES
## Note this version does not overwrite existing files

path = "G:\\Projects\\PIHM_R\\Test\\"
projectname = "projectname"
Resultpath = path  #Somewhere else if desire

#############################################################

len_path = nchar(path)
len_projectname  = nchar(projectname )
len_Resultpath = nchar(Resultpath )

if(len_path < 1)
{
  print("Invalid path")
  stop()
}
if(len_projectname < 1)
{
  print("Invalid projectname")
  stop()
}
if(len_Resultpath < 1)
{
  print("Invalid Resultpath")
  stop()
}

#############################################################

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

#############################################################

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

#############################################################

