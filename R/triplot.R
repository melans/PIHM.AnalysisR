#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#' =============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#' =============================================
#' @param  Path of output folder.
#' @keywords read output
#' @export  output data.
#' @examples
#' PIHM()
 
PIHM.triplot<-function(data,terrain=FALSE, zratio=0,dist=0,shift=0){
    library(rgl)
    mesh <- readmesh();
    riv <- readriv();
    msh <- mesh$mesh;
    pts <- mesh$points;
    tri <- msh[,2:4];
    m <- nrow(tri);
    x=t(matrix(c(pts[tri[,1],2],pts[tri[,2],2],pts[tri[,3],2]),m,3) );
    y=t(matrix(c(pts[tri[,1],3],pts[tri[,2],3],pts[tri[,3],3]),m,3) );
    z=t(matrix(c(pts[tri[,1],4],pts[tri[,2],4],pts[tri[,3],4]),m,3) );
    H=t(matrix( (pts[msh[,2],4]+pts[msh[,3],4]+pts[msh[,4],4] )/3, m,3))
    #H=(pts[msh[,2],4]+pts[msh[,3],4]+pts[msh[,4],4])/3
    dhdx <- diff(Hlim) / min(diff(xlim),diff(ylim));
    zr <- 1;
    if (dhdx < 1/2000 ) {
        zr <-  round(1/2000/dhdx) ;
        warning('\n\tdh/dx = ',round(dhdx,digits=5),'.\tZ ratio > ', zr, ' is recommended');
        if (zratio <= 0){
            zr <- zr;
        }else{
            zr <- zratio;
        }
    }
    z=z*zr;
    Hlim <- range(H)
    Hlen <- Hlim[2] - Hlim[1] + 1
    colorlut <- terrain.colors(Hlen,alpha=0) # height color lookup table
    col <- colorlut[ H-Hlim[1]+1 ] # assign colors to heights for each point
    
    open3d()
    triangles3d(x,y,z,color=col)
    

}
