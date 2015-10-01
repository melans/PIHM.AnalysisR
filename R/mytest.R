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
 
mytest<-function(data,terrain=FALSE, zratio=0){
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
   Hlim <- range(H)
    xlim<- range(x);
    ylim<-range(y)
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
    colorlut <- terrain.colors(Hlen,alpha=1) # height color lookup table
    col <- colorlut[ H-Hlim[1]+1 ] # assign colors to heights for each point
    

    
   # open3d()
    rgl.open();
    triangles3d(x,y,z,color=col,box=TRUE)
    legend3d("topright", legend = paste(1:90), pch = 1:90, col = colorlut, cex=1)
    
}



newtest <-function(){
    x=1:100*1000;
    y=1:100*1000;
    z=matrix(sort(abs(rnorm(100*100,500,300) )),100,100)/1e4
    cat(range(z))
    H <- z
    Hlim <- round(range(H[!is.na(H)]))
    if(diff(Hlim)<1 ) {    
        Hlim <- round(100*range(H[!is.na(H)]))
    }

    Hlen <- Hlim[2] - Hlim[1] + 1
    colorlut <- terrain.colors(Hlen,alpha=1) # height color lookup table
    colorlut <- terrain.colors(Hlen) # height color lookup table

    col <- colorlut[ H-Hlim[1]+1 ] # assign colors to heights for each point
       
   surface3d(x,y,z,color=col);
   aspect3d(x=1,y=1,z=.5)
   axis3d('x',labels=T);
   axis3d('y',labels=TRUE);
   axis3d('Z',lables=T);
     bgplot3d( suppressWarnings ( image.plot( legend.only=TRUE,  zlim=Hlim/100,col=colorlut) ) )#legend

   
    #bgplot3d(   image.plot( legend.only=TRUE, legend.args=list(text='Value'), zlim=Hlim,col=colorlut) ) 
   
   #persp3d(x,y,z,color=col,nomal_z=10);
   

}
testhydrograph <- function(P,Q){
    prcp=readprcp() # forc$PRCP$ts1
    time(prcp)=round(time(prcp),units='days');
    pdaily=apply.daily(prcp,FUN=mean);

    q=readout(ext='rivFlx1',binary=TRUE);
    riv <-readriv(bak=TRUE)
    outlets <- riv$River$outlets
    Q <- q[,outlets];

    t=time(Q);
    P=pdaily[t];


    twoord.plot(t,Q,t,P,lylim=range(Q),rylim=range(P),lcol='blue',rcol='yellow',type=c('l','bar'))



    df.bar <- barplot(P,col='blue',ylim=rev(range(P)),axes=FALSE,yaxt='n');
    axis(side=4)
    lines(x=df.bar, y=Q,ylim=range(Q));
    


}

