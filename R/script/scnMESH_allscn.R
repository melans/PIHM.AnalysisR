Dat0=readRDS('gu0.RDS')
Dat1=readRDS('gu1.RDS')
Dat2=readRDS('gu2.RDS')
Dat2=readRDS('gu3.RDS')
Dat2=readRDS('gu4.RDS')

ncell=ncol(Dat0)

mcalib=readRDS('calib.RDS')
pt=mcalib[17:18,]

pid=which(pt[2,]==0)    #which temp is 0, in calib 
tid=which(pt[1,]==1)    #which prcp is 1, in calib 



att0=readatt(attfile='lc.att_Pre')
att1=readatt(attfile='lc.att$')
att2=readatt(attfile='lc.att_Urban3')
lcs=cbind(att0[,4], att1[,4], att2[,4]) 
lcfiles=paste(projectname,'_LC',c(1:3),'.png', sep='');

tlist=c(0,1,3)
plist=c(0.9, 1, 1.1)
mpt=t(expand.grid(list(plist,tlist)))
rownames(mpt)=c('FactorP','DeltaT')
n=ncol(mpt);
cid = numeric(n);
for (i in 1:n){
    cid[i]=which(vector.identical(pt,mpt[,i],2))
}

ref.map=Dat1[cid[2],]

scns=list()

scns[[1]] = Dat0[cid, ] - t(matrix(ref.map, ncol=n, nrow=ncell))
scns[[2]] = Dat1[cid, ] - t(matrix(ref.map, ncol=n, nrow=ncell))
scns[[3]] = Dat2[cid, ] - t(matrix(ref.map, ncol=n, nrow=ncell))
for (i in 1:3){
    tmp=scns[[i]]
    tmp[which( abs(tmp) >20) ]=0
    scns[[i]]=tmp
}
Hlim=range(unlist(lapply(scns, FUN=range, na.rm=TRUE)))
Hlim[1]=floor(Hlim[1])
Hlim[2]=ceiling(Hlim[2])

for (j in 1:3){
    #pngfiles=paste(projectname,'scn_',j,'_GU_',cid,'.png',sep='')
    pngfiles=paste(projectname,'scn_',j,'_GU_',cid,'.asc',sep='')

    lc=lcs[,j]
   #  PIHM.triplot(lc, Hlim=c(11,95), name='NLCD', title='', fn=lcfiles[j],colorFUN=rainbow)# colormap=lc.colors)

    for (i in 1:n){
        
        tmp = (mpt[1,i]-1)
        ttl=character(2)
        ttl[1]=paste((mpt[1,i]-1)*100,'%')
        ttl[2]= paste( mpt[2,i] ,'C')
            if (tmp>=0){sign=' +'}else{sign=' '}
        
        titlename= paste(t(c('P', 'T')), t(c(sign, ' +')),t(ttl), sep='')
    tmp = InterpSpatial(data=scns[[j]][i,], elevation=0, res=100)
    fn =pngfiles[i] 
    writeASC(data = tmp, fn=fn)
        #PIHM.triplot(scns[[j]][i,], Hlim=Hlim, name='Storage', title='', fn=pngfiles[i], colorFUN=topo.colors, colorreverse=TRUE)
#        PIHM.3Dmap(scns[[j]][i,], Hlim=Hlim, name='Storage', title=paste(titlename,collapse='   '), fn=pngfiles[i], colorFUN=topo.colors, colorreverse=TRUE)
    }
#    PIHM.3Dclose()
}


