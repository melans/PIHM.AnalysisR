
spihm()

M=200
x=seq(0.001, 1, length.out=M);
N=10;
K=5 #m/day


BETA=seq(1.1,1.5,length.out=10)*1.5
y=matrix(0,ncol=N,nrow=M)
for(i in 1:N){
    y[,i]=VGt2h(alpha=1,beta=BETA[i], se=x,minpsi=-70)#15e10);
}
col=terrain.colors(N)
matplot(abs(y),x, type='l',log='x', lty=1, col=col, ylab='Effective Saturation', xlab='-H')
leglines=paste('Beta=',round(BETA,3),sep='')
legend('bottomleft',legend=leglines, lty=1, col=col)

Keff=matrix(0,M,N)
for(i in 1:N){
    Keff[,i]=K*satKfunc(x,BETA[i])
}
col=terrain.colors(N)
matplot(abs(Keff),x, type='l',log='x', lty=1, col=col, ylab='Effective Saturation', xlab='K(se)')
leglines=paste('Beta',round(BETA,3),sep='')
legend('bottomleft',legend=leglines, lty=1, col=col)

Q=Keff*y
col=terrain.colors(N)
matplot(abs(Q),x, type='l',log='x', lty=1, col=col, ylab='Effective Saturation', xlab='Flow Rate')
leglines=paste('Beta',round(BETA,3),sep='')
legend('bottomleft',legend=leglines, lty=1, col=col)






Alpha=seq(1,15,length.out=10)
y=matrix(0,ncol=N,nrow=M)
for(i in 1:N){
    y[,i]=VGt2h(alpha=Alpha[i],beta=1.5, se=x,minpsi=-70)#15e10);
}
col=terrain.colors(N)
matplot(abs(y),x, type='l',log='x', lty=1, col=col, ylab='Effective Saturation', xlab='-H')
leglines=paste('Alpha=',round(Alpha,3),sep='')
legend('bottomleft',legend=leglines, lty=1, col=col)

