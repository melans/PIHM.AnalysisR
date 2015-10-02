#############################################################
# Notes
# Assumption is to specify wdir location of scripts
#
# To start Lele's Tool
# source("KFunction_Alone.R")
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


#############################################################
len_path = nchar(path)
if(len_path < 1)
{
  print("Invalid path")
  stop()
}
#############################################################


KFunc <- function( ks=5, km=500, alpha=5, beta=1.5, af=0.01, sd=20, y0=0.01){
Dinf=0.1
    
    z=sd
    if(length(ks) >1){
        key='Kinf'
        ff=ks
    }else if (length(km)>1 ){
        key='Kmp'
        ff=km
    }else if (length(alpha)>1 ){
        key='alpha'
        ff=alpha
    }else if (length(beta)>1 ){
        key='beta'
        ff=beta
    }else if (length(af)>1 ){
        key='AF'
        ff=af
    }else if (length(sd)>1 ){
        key='Depth'
        ff=sd
    }else if(length(y0)>1){
        key='Y0'
        ff=y0
    }else {
        return(0)
    }
    mat=expand.grid(ks,km,alpha,beta,af,sd,y0)
    ne=100
    nf=nrow(mat)
    
    mke=matrix(0,nrow=ne,ncol=nf)
    mQ=matrix(0,nrow=ne,ncol=nf)
    mG=matrix(0,nrow=ne,ncol=nf)

    for(i in 1:nf){
        z=mat[i,6]
        Yu=seq(0.1, z-Dinf, length.out=ne)
        xe=Yu/z
        ke=KVs(ks=mat[i,1], km=mat[i,2], alpha=mat[i,3], beta=mat[i,4],
               af=mat[i,5],Ysurf=mat[i,7],
                  De=z, Yu=Yu, if.save=FALSE,if.plot=FALSE)
        mke[,i]=ke[,2]
        mG[,i]=ke[,3]
        mQ[,i]=ke[,5]
    }

    yname=paste(key,ff)
 
    library(ggplot2)
 
    imagecontrol(path, fn=paste(key,'.jpg'), wd=30,ht=24,res=100 ,bg='grey')

    par(mfrow=c(2,2))
    xlab='Effective Saturation'
    col=terrain.colors(nf)
    matplot(x=xe,y = mke, log='y', ylab = 'Effective K',xlab='Se', main = paste('Effective Conductivity vs.',key),  type='l', col = col)
    matplot(x=xe,y = mG, log='y', ylab = 'Gradient',xlab='Se', main = paste('Gradient vs.',key), type='l', col = col)
    matplot(x=xe,y = mQ, log='y', ylab = 'Flow rate',xlab='Se', main = paste('Flow rate vs.',key),  type='l', col = col)
    plot.new()
    legend(x = "topright", legend = yname,  lty = 1,col =col)
    info=t(matrix(c('Ksat=',ks[1], 'Kmp=',km[1],'Deficit=', z[1],
                    'Alpha=',alpha[1], 'Beta=', beta[1], 'Af=', af[1],
                    'Ysurf=', y0[1], '','','N=',nf,'max=',max(ff)), nrow=2))
    
    leglines=paste(info[,1], info[,2])
    legend('topleft',leglines,bg="transparent")    
    dev.off()
    
}

#############################################################

KFunc(beta=seq(1.2,15,by=0.2))
KFunc(alpha=seq(2,100,length.out=10) )
KFunc(ks=seq(1,10,length.out=10) )
KFunc(km=seq(10,1000,length.out=10) )
KFunc(af=seq(0.01,0.5,length.out=20) )
KFunc(y0=seq(0, 1,length.out=10) )

#############################################################
