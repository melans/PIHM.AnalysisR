#############################################################
# Notes
# Assumption is to specify wdir location of scripts
#
# To start Lele's Tool
# source("KmpKmx_Alone.R")
#
# See Notes for packages needed
#############################################################

## NEED THE FOLLOWING R FILES
source("pihm.plot.R")

#############################################################

Dinf=0.1
ks= 37.9    #m/day
km=606.6    #m/day
alpha=6.58 * 1.5
beta=1.238 *1.15
af=0.01 * 1

se=10:900/1000

kf= (se)^0.5 * (-1+( 1- se^(beta/(beta-1))) ^  ((beta -1)/beta) )^2 ;

fks=ks * kf
fkm=km * kf

fkms= km * af + ks * (1- af) * kf

plot(se,fkm, ylab=expression(paste('K(',theta,')   [L/T]')),col='green',
     log='y', lty=1, type='l', lwd=3)

lines(se,fks, col='blue', lwd=3)
lines(se,fkms, col='red', lwd=3)
col=c('green','blue', 'red')
legend('right',legend=c('Kmp', 'Ks', 'Keff'),lwd=3, lty=1, col=col)



