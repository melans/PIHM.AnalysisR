source('/gpfs/home/lzs157/work/PIHM/PIHM.AnalysisR/R/script/waterbalance.R')

qgp=Ycfsub/YcP * 100;

r=colMeans(qgp)

qi= PIHMOUT$infil[t];
xqi=colSums(qi)
PIHM.3Dtriplot(data=xqi)

qr= PIHMOUT$Rech[t];
xqr=colSums(qr)
PIHM.3Dtriplot(data=xqr)

