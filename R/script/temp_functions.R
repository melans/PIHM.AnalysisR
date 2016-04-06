rescale <- function(freq, lQ.thr, hQ.thr){
    x=freq 
    mQ.id = which(x<lQ.thr & x>hQ.thr) # medium flow.
    x[mQ.id]=(x[mQ.id]-hQ.thr) *0.4 /(lQ.thr-hQ.thr) + 0.3
    
    hQ.id=which(x<=hQ.thr)  # flow blow 0.1
    x[hQ.id]=x[hQ.id]*3 # expand to 0~0.3
    lQ.id=which(x>=lQ.thr) # low flow
    x[lQ.id]=(x[lQ.id]-lQ.thr) * 3 + 0.7
    
    return(x)
}
scnlog <- function (Q0=Q0,Q1=Q1,Q2=Q2,att2=att2, file='scncompare_log.png'){

lQ.thr=0.90
hQ.thr=0.05

warmupyears=2;
att0=readatt(attfile='lc.att_Pre')
att1=readatt(attfile='lc.att$')


t=time(Q0);
starttime=as.POSIXlt(paste(year(time(Q1)[1])+warmupyears,'-01-01', sep=''))

q0=Q0[which(t>starttime)]
q1=Q1[which(t>starttime)]
q2=Q2[which(t>starttime)]


mcalib=readRDS('calib.RDS')
pt=mcalib[17:18,]

pid=which(pt[2,]==0)    #which temp is 0, in calib 
tid=which(pt[1,]==1)    #which prcp is 1, in calib 


tlist=c(0,1,3)
plist=c(0.9, 1, 1.1)

tlist=c(0)
plist=c( 1)

mpt=t(expand.grid(list(plist,tlist)))
rownames(mpt)=c('FactorP','DeltaT')
n=ncol(mpt);
cid = numeric(n);
for (i in 1:n){
    cid[i]=which(vector.identical(pt,mpt[,i],2))
}
QFDC=list()
QQ=list()
for (i in 1:n){
    qq=cbind(as.numeric(q0[,cid[i]]),as.numeric(q1[,cid[i]]),as.numeric(q2[,cid[i]]))
    QQ[[i]]=qq;
    qfdc=fdc(qq,plot=FALSE)
    QFDC[[i]]=qfdc
}
saveRDS(QQ,file=paste(file,'Q.RDS'))
saveRDS(QFDC,file=paste(file,'QFDC.RDS'))



imagecontrol(path='./', fn=file, wd=30, ht=25, res=100)
par(mfrow=c(length(tlist),length(plist)))
col=c('green','blue','red')
lty=c(2,1,3)
pch=c(1,1,1)
lwd=c(2,2,2)
#ylim=c(round(min(q0,q1,q2, na.rm=TRUE),1) ,ceiling(max(q0,q1,q2, na.rm=TRUE)/100)*100 )
ylim=c(50 ,ceiling(max(q0,q1,q2, na.rm=TRUE)/100)*100 )
xlim=c(1e-4, 0.1)

for (i in 1:n){
    ref.q=QQ[[2]][,2]
    ref.fdc=QFDC[[2]][,2]
    ref.ord= order(ref.fdc)
    ref.x=ref.fdc[ref.ord]  # rescale(ref.fdc[ref.ord] ,lQ.thr=lQ.thr, hQ.thr=hQ.thr)
    if (i %% 3 ==1){ ylab='Discharge'
    }else{ ylab='' }
    if (i >6 ){ xlab='Frequency'
    }else{ xlab='' }
    
#ref       
    tmp = (mpt[1,i]-1)
    ttl=character(2)
    ttl[1]=paste((mpt[1,i]-1)*100,'%')
    ttl[2]= paste( mpt[2,i] ,'C')
    if (tmp>=0){sign=' +'}else{sign=' '}
    plot(ref.x,ref.q[ref.ord], col='black', type='l',
         log='xy', ylim=ylim, xaxt = "n", xlim=xlim,
         xlab=xlab, ylab=ylab,
         main=paste(t(c('P', 'T')), t(c(sign, '+')),t(ttl), sep='')
         )
    if (i %% 3 ==1){    #y axis
        y.at=10^seq(-1,log(ceiling(ylim[2])), by=1)
        axis(side=2,at=y.at,col='blue',tck=-0.02)
    }
#scn
    qq=QQ[[i]]
    qfdc=QFDC[[i]]
    ord= apply(qfdc,2,order)
#scn0    
    sid=1;
    oid=ord[,sid]
    x=qfdc[oid,sid]  # rescale(qfdc[oid,sid],lQ.thr=lQ.thr, hQ.thr=hQ.thr)
    lines(x,qq[oid,sid],lty=lty[sid],col=col[sid], lwd=lwd[sid], pch=pch[sid])
#scn1        
    sid=2;
    oid=ord[,sid]
    x=qfdc[oid,sid]  # rescale(qfdc[oid,sid],lQ.thr=lQ.thr, hQ.thr=hQ.thr)
    lines(x,qq[oid,sid],lty=lty[sid],col=col[sid], lwd=lwd[sid], pch=pch[sid])

#scn2        
    sid=3;
    oid=ord[,sid]  
    x=qfdc[oid,sid]  #  rescale(qfdc[oid,sid],lQ.thr=lQ.thr, hQ.thr=hQ.thr)    
    lines(x,qq[oid,sid],lty=lty[sid],col=col[sid], lwd=lwd[sid], pch=pch[sid])
    if (i > 6){
        x.at=sort(c(0,1e-4, 1e-3, 1e-2, 1e-1) )
        x.tick=x.at #c(0,hQ.thr,lQ.thr,1)
        axis(side=1,at=x.at,x.tick,col='red',tck=-0.02)
    }
    grid()    
}
legnames=c('Reference', 'Pre-history','Current', 'Over-developed')
legend('topright',legnames, lty=c(1,lty), col=c('black',col),lwd=c(1,lwd))

dev.off()
}


fdcplot <- function (Q,QFDC){
 ylab='Discharge'
 xlab='Frequency'

    
#ref       
    ttl=character(2)
    ttl[1]=paste((mpt[1,i]-1)*100,'%')
    ttl[2]= paste( mpt[2,i] ,'C')
    if (tmp>=0){sign=' +'}else{sign=' '}
    plot(ref.x,ref.q[ref.ord], col='black', type='l',
         log='xy', ylim=ylim, xaxt = "n", xlim=xlim,
         xlab=xlab, ylab=ylab,
         main=paste(t(c('P', 'T')), t(c(sign, '+')),t(ttl), sep='')
         )
    if (i %% 3 ==1){    #y axis
        y.at=10^seq(-1,log(ceiling(ylim[2])), by=1)
        axis(side=2,at=y.at,col='blue',tck=-0.02)
    }
 if (i %% 3 ==1){    #y axis
        y.at=10^seq(-1,log(ceiling(ylim[2])), by=1)
        axis(side=2,at=y.at,col='blue',tck=-0.02)
    }
#scn
    qq=QQ[[i]]
    qfdc=QFDC[[i]]
    ord= apply(qfdc,2,order)
#scn0    
    sid=1;
    oid=ord[,sid]
    x=qfdc[oid,sid]  # rescale(qfdc[oid,sid],lQ.thr=lQ.thr, hQ.thr=hQ.thr)
    lines(x,qq[oid,sid],lty=lty[sid],col=col[sid], lwd=lwd[sid], pch=pch[sid])
#scn1        
    sid=2;
    oid=ord[,sid]
    x=qfdc[oid,sid]  # rescale(qfdc[oid,sid],lQ.thr=lQ.thr, hQ.thr=hQ.thr)
    lines(x,qq[oid,sid],lty=lty[sid],col=col[sid], lwd=lwd[sid], pch=pch[sid])

#scn2        
    sid=3;
    oid=ord[,sid]  
    x=qfdc[oid,sid]  #  rescale(qfdc[oid,sid],lQ.thr=lQ.thr, hQ.thr=hQ.thr)    
    lines(x,qq[oid,sid],lty=lty[sid],col=col[sid], lwd=lwd[sid], pch=pch[sid])
    if (i > 6){
        x.at=sort(c(0,1e-4, 1e-3, 1e-2, 1e-1) )
        x.tick=x.at #c(0,hQ.thr,lQ.thr,1)
        axis(side=1,at=x.at,x.tick,col='red',tck=-0.02)
    }
    grid()    
}
