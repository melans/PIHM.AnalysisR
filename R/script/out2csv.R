#output PIHM data for CNH data.

#vnames=names(PIHMOUT)
vnames=c( "IS","snow","surf","GW","unsat","ET0","ET1","ET2","infil","Rech") 
snames=c( "Interception(m)","snow(m)","Surf_Storage(m)","GW_Storage(m)","unsat_Storage(m)",
         "InterceptionEvp(m/s)","Transpiration(m/s)","Evapration(m/s)","infil(m/s)","Recharge(m/s)") 

et = as.POSIXlt('1997-01-01', tz='UTC')
tt=t[which(t>=sut & t<et)]
nv=length(vnames)
ncell=1663
nt=length(tt)
cids=as.numeric(t(matrix(1:ncell, nrow=ncell, ncol = nt)))
M=nt * ncell;
N = nv+2
mat = matrix(0, nrow=M, ncol=N)
mat[,1]=cids
mat[,2]=as.numeric(strftime(tt,format='%Y%m%d'))
colnames(mat)=c('MeshCell','Date',snames)
for (i in 1:nv){
    mat[,i+2]=as.numeric(PIHMOUT[[vnames[i]]][tt,])
}
dmat=as.data.frame(mat);
dmat[,2]=as.Date(tt)
write.csv(file='LancasterData.csv',x=dmat,quote=FALSE, row.names=FALSE)


