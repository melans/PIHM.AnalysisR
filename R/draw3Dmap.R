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
 
PIHM.3Dmap<-function(data,terrain=FALSE, zratio=0,dist=0){
    library(rgl)
    mesh <- readmesh();
    riv <- readriv();
    msh <- mesh$mesh;
    pts <- mesh$points;
    x=pts[msh[,2],2]+pts[msh[,3],2]+pts[msh[,4],2];
    y=pts[msh[,2],3]+pts[msh[,3],3]+pts[msh[,4],3];
    z=pts[msh[,2],4]+pts[msh[,3],4]+pts[msh[,4],4];
    xlim <- range(x);
    ylim <- range(y);
    dx=(xlim[2]-xlim[1])/200;
    dy=dx; #(ylim[2]-ylim[1])/100;
    xc <- seq(xlim[1], xlim[2],by=dx);
    yc <- seq(ylim[1], ylim[2],by=dy);
    if (missing('data')){
        mat <- interp(x,y,z,xo=xc,yo=yc);
        H=mat$z;
    }else{
        mdata <-interp(x,y,data,xo=xc,yo=yc);
        if (terrain){
            mat <- interp(x,y,z,xo=xc,yo=yc);
            H <- mat$z + mdata$z;   # Terrain + spatial data
        }else{
            H <- mdata$z;
        }
    }
    
    Hlim <- round(range(H[!is.na(H)]))
    dhdx <- diff(Hlim) / min(diff(xlim),diff(ylim));
    zr <- 1;
    if (dhdx < 1/10 ) {
        zr <-  round(1/10/dhdx) ;
        warning('\n\tdh/dx = ',round(dhdx,digits=5),'.\tZ ratio > ', zr, ' is recommended');
        if (zratio <= 0){
            zr <- zr;
        }else{
            zr <- zratio;
        }
    }

    H <-  H*zr;
    Hlim <- round(range(H[!is.na(H)]))
    Hlen <- Hlim[2] - Hlim[1] + 1
    colorlut <- terrain.colors(Hlen,alpha=0) # height color lookup table
    col <- colorlut[ H-Hlim[1]+1 ] # assign colors to heights for each point
    
    open3d()
#    surface3d(xc, yc, H, coord=1:3, color=col, alpha=0.75, back="lines")
    terrain3d(xc, yc, H, coord=1:3, color=col, alpha=0.75, back="lines")
    grid3d(c("x", "y+", "z"))
    #rgl.surface(yc, xc, H, coord=1:3, color=col, alpha=0.75, back="lines")
    
    colorlut <- heat.colors(Hlen,alpha=1) # use different colors for the contour map
    col <- colorlut[ H-Hlim[1]+1 ] 
    if (dist ==0){
        dist=mean(data)-diff(Hlim);
    }else{
        dist <- dist;
    }
    
    surface3d(xc, yc, matrix(dist, nrow(H), ncol(H)),color=col, back="fill")
    
    return(diff(Hlim));
}
