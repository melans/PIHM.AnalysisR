FieldCapacity <- function(alpha=2, beta=1.3, zs=20, s2, if.plot=TRUE){
if(missing('s2')){
    s2=seq(0,zs,length.out=100)
}
   n=beta
  deficit = zs - s2; 
sfc = (1+ (alpha * deficit)^( -n ) )  ^ ( -1 / n ) / (alpha  ) ;
sfc=((alpha *deficit)^(-beta) +1) ^(-1/beta)/alpha
theta= sfc/deficit
ylim=range(c(sfc,theta),na.rm=TRUE);
#par(mfrow=c(2,1))
if(if.plot){
    matplot(deficit,cbind(sfc,theta), ylim=ylim,type='l', lty=1,lwd=3,col=c('blue','red'));
    legend(1/3*max(s2), 8/9*ylim[2],c('Storage [L]', 'Saturation [1]'),lty=1,col=c('blue','red'), lwd=3)
    grid()
}
xx=(sfc-deficit)
id=which(xx>0)
if(length(id)>0){
    warning('Sfc > deficit', id);
}
ret= list('s2'=s2,'sfc'=sfc, 'de'=deficit)
return(ret)
}

fcs <- function(){
alpha=2:10;
beta =seq(1.2, 5,by=0.5)
zs=2:20

nv=length(alpha)
name='alpha'
for (i in 1:nv){
    a=alpha[i]
    tmp=FieldCapacity(alpha=a, beta=1.5, zs=10, if.plot=FALSE)
    if(i==1){
        x=tmp$de
        y=tmp$sfc
    }else{
        x=cbind(x,tmp$de)
        y=cbind(y,tmp$sfc)
    }
}
z=y/x
col= terrain.colors(nv)
image.control(path=Resultpath, fn=paste(name,'.png'))
matplot(x,y, type='l',col=col, lty=1, lwd=2, 
        xlab='Deficit', ylab='S1')
leglines = paste(name,'=',alpha)
legend('topleft',leglines,bg="transparent", lty=1, col=col)
grid()
dev.off()
#===========
nv=length(beta)
name='beta'
for (i in 1:nv){
    a=beta[i]
    tmp=FieldCapacity(alpha=2, beta=a, zs=10, if.plot=FALSE)
    if(i==1){
        x=tmp$de
        y=tmp$sfc
    }else{
        x=cbind(x,tmp$de)
        y=cbind(y,tmp$sfc)
    }
}
z=y/x
col= terrain.colors(nv)
image.control(path=Resultpath, fn=paste(name,'.png'))
matplot(x,y, type='l',col=col, lty=1, lwd=2, 
        xlab='Deficit', ylab='S1')
leglines = paste(name,'=',beta)
legend('topleft',leglines,bg="transparent", lty=1, col=col)
grid()
dev.off()


}

