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
 
PIHM.rivplot<-function(data,terrain=FALSE, zratio=0,dist=0,shift=0){
    library(rgl)
    mesh <- readmesh();
    riv <- readriv();
    msh <- mesh$mesh;
    pts <- mesh$points;
    #=====plot river segments==========
    seg <- riv$River$riv[,c('FROM','TO')]
    
    m <-nrow(seg)
    x=t(matrix(c(pts[seg[,1],2],pts[seg[,2],2]),m,2) );
    y=t(matrix(c(pts[seg[,1],3],pts[seg[,2],3]),m,2) );
    z=t(matrix((pts[seg[,1],5]+pts[seg[,2],5])/2,m,2) )*zr;
    
    if(shift <=0){
        moveup =t(matrix( abs(diff(z)),m,2));
    }        
    
    z=z+moveup;
    lwidth <- t(matrix( riv$River$riv[,'SHAPE'] ,m,2))


     Hlim <- range(lwidth)
    Hlen <- Hlim[2] - Hlim[1] + 1
    colorlut <- heat.colors(Hlen,alpha=0.5) # height color lookup table
    col <- colorlut[ lwidth-Hlim[1]+1 ] # assign colors to heights for each point
    
    segments3d(x,y,z,color=col,lwd=6)


}
