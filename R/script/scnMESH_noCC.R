
scnMesh_NoCC <- function(dir='PIHMAnalysis/', name = 'GW', if.ele = 0, res=100, maxvalue=20){

    # -1 --bed; 0-- no DEM; 1- surface DEM
nvalue = 5;
#dir='PIHMAnalysis/'
#name = 'GW'
#ext='_DEM'
#vname=paste(name, ext,sep='');
#if.ele = -1 ;   # -1 --bed; 0-- no DEM; 1- surface DEM
#nvalue = 5;
if(if.ele <0){
    ext = '+bedele'
}else if(if.ele>0){
    ext = '+surfele'
}else{
    ext =''
}
vname=paste(name, ext,sep='');
for (i in 1:(nvalue)){
    rdata=readRDS(paste(name,i-1,'.RDS',sep=''))
    if(i<=1){
        Dat=rdata
    }else{
        Dat=abind(Dat, rdata, along=3)
    }
}
rivid=c(107,206,62,239)
ncell=ncol(Dat)

mcalib=readRDS('calib.RDS')
pt=mcalib[17:18,]

pid=which(pt[2,]==0)    #which temp is 0, in calib 
tid=which(pt[1,]==1)    #which prcp is 1, in calib 

att0=readatt(attfile='lc.att_Pre')
att1=readatt(attfile='lc.att$')
att2=readatt(attfile='lc.att_Urban3')
lcs=cbind(att0[,4], att1[,4], att2[,4]) 
lcfiles=paste(projectname,'_LC',c(1:3),'.png', sep='');
tlist=c(0)
plist=c(1)

mpt=t(expand.grid(list(plist,tlist)))
rownames(mpt)=c('FactorP','DeltaT')
n=ncol(mpt);
cid = numeric(n);
for (i in 1:n){
    cid[i]=which(vector.identical(pt,mpt[,i],2))
}


for( i in 1:nvalue){ #Interpolate and write ASC map out.
    scn = Dat[cid, , i]
    scn[which( abs(scn) > maxvalue) ]=0

    tmp = InterpSpatial(data=scn, elevation=if.ele, res=res)
    fn = paste(dir,vname,'_', i-1 , '.asc',sep='')
    writeASC(data = tmp, fn=fn)
}

}

scnMesh_NoCC(name='gu', if.ele=-1)
scnMesh_NoCC(name='gu')
scnMesh_NoCC(name='GW', if.ele=-1)
scnMesh_NoCC(name='GW')
tmp = InterpSpatial(res=100)
   fn = paste(dir,'DEM.asc',sep='')
    writeASC(data = tmp, fn=fn)

