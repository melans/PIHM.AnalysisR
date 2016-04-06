#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;

LinearSoilDepth <- function( mu=10, sigma=1, savemesh=FALSE){

    SdRange=4
    mesh=readmesh()
    pt=mesh$points
    ele=pt[,5]
    oid=order(ele)

    dep=rnorm(mean=mu, sd=sigma,n=length(ele))
    dep[which(dep<=0)]=0;
    sid=order(dep,decreasing=TRUE)
    sd=dep[sid]
   # sd=-SdRange*ele[oid]/max(ele)+12
 #   par(mfrow = c(2, 1))
#    plot(sd,ylab='soil thickness')
#    plot(ele[oid],ylab='elevation')
    ylim=c(min(ele[oid]-sd),max(ele));

    plot(ele[oid],ylab='elevation', ylim=ylim,type='l', col='red')
    lines(ele[oid]-sd,col='blue')

pt[,4]=pt[,5]-sd
mesh$points=pt;
if(savemesh){
    writemesh(mesh)
}
return(sd)

}
