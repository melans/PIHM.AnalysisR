#############################################################
# Notes
# Assumption is to specify wdir location of scripts
#
# To start Lele's Tool
# source("AlphaBeta_Alone.R")
#
# See Notes for packages needed
#############################################################
## NEED THE FOLLOWING R FILES
####################

source("pihm.plot.R")
source("KV_effective.R")

#############################################################
path = "G:\\Projects\\PIHM_R\\Test\\"

K=5 #m/day

#############################################################
# Van Genuchten Soil Parameter BETA PROPERTIES
#############################################################
beta_H_fname = "BetaH.png"
beta_K_fname = "BetaK.png"
beta_Q_fname = "BetaQ.png"
beta_start = 1.1
beta_end = 1.5
beta_calib = 1.5

#############################################################
# Van Genuchten Soil Parameter ALPHA PROPERTIES
#############################################################
alpha_fname = "Alpha.png"
alpha_start = 1.0
alpha_end = 15.0
alpha_calib = 1.0

alpha_beta_input =1.5

#############################################################

len_path = nchar(path)
if(len_path < 1)
{
  print("Invalid path")
  stop()
}

#############################################################

M=200
x=seq(0.001, 1, length.out=M);
N=10;

col=colors()[c(261,262,263)]; 
#col= terrain.colors(N)

#############################################################

BETA=seq(beta_start,beta_end,length.out=10)*beta_calib
#BETA=seq(1.1,1.5,length.out=10)*1.5

y=matrix(0,ncol=N,nrow=M)

for(i in 1:N){
    y[,i] = VGt2h(alpha=1,beta=BETA[i], se=x,minpsi=-70)#15e10);
}

#############################################################

fname = paste(path,beta_H_fname)
png(fname, width = 800, height = 600)

matplot(abs(y),x, type='l',log='x', lty=1, col=col, ylab='Effective Saturation', xlab='-H')
leglines=paste('Beta=',round(BETA,3),sep='')
legend('bottomleft',legend=leglines, lty=1, col=col)
dev.off()

Keff=matrix(0,M,N)

for(i in 1:N){
    Keff[,i]=K*satKfunc(x,BETA[i])
}

#############################################################

fname = paste(path,beta_K_fname)
png(fname, width = 800, height = 600)

matplot(abs(Keff),x, type='l',log='x', lty=1, col=col, ylab='Effective Saturation', xlab='K(se)')
leglines=paste('Beta',round(BETA,3),sep='')
legend('bottomleft',legend=leglines, lty=1, col=col)
dev.off()

#############################################################

fname = paste(path,beta_Q_fname)
png(fname, width = 800, height = 600)

Q=Keff*y

matplot(abs(Q),x, type='l',log='x', lty=1, col=col, ylab='Effective Saturation', xlab='Flow Rate')
leglines=paste('Beta',round(BETA,3),sep='')
legend('bottomleft',legend=leglines, lty=1, col=col)

dev.off()

#############################################################

#Alpha=seq(1,15,length.out=10)
Alpha=seq(alpha_start,alpha_end,length.out=10)*alpha_calib

fname = paste(path,alpha_fname)
png(fname, width = 800, height = 600)

y=matrix(0,ncol=N,nrow=M)
for(i in 1:N){
    #y[,i]=VGt2h(alpha=Alpha[i],beta=1.5, se=x,minpsi=-70)#15e10);
    y[,i]=VGt2h(alpha=Alpha[i],beta=alpha_beta_input, se=x,minpsi=-70)#15e10);
}

matplot(abs(y),x, type='l',log='x', lty=1, col=col, ylab='Effective Saturation', xlab='-H')
leglines=paste('Alpha=',round(Alpha,3),sep='')
legend('bottomleft',legend=leglines, lty=1, col=col)

dev.off()

#############################################################


