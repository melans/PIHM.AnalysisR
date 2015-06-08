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
 
PIHM.3Dmap<-function(data,terrain=FALSE,name='value',zratio=0,dist=0,fn='3Dmap.png',path=Resultpath){
    mesh <- readmesh(bak=TRUE);
    riv <- readriv(bak=TRUE);
    msh <- mesh$mesh;
    pts <- mesh$points;
    x=(pts[msh[,2],2]+pts[msh[,3],2]+pts[msh[,4],2])/3;
    y=(pts[msh[,2],3]+pts[msh[,3],3]+pts[msh[,4],3])/3;
    z=(pts[msh[,2],4]+pts[msh[,3],4]+pts[msh[,4],4])/3;
    xlim <- range(x);
    ylim <- range(y);
    dx=(xlim[2]-xlim[1])/200;
    dy=dx; #(ylim[2]-ylim[1])/100;
    xc <- seq(xlim[1], xlim[2],by=dx);
    yc <- seq(ylim[1], ylim[2],by=dy);
    zname='Elevation'
    if (missing('data')){
        mat <- interp(x,y,z,xo=xc,yo=yc);
        H=mat$z;
        vname=zname;
    }else{
        mdata <-interp(x,y,data,xo=xc,yo=yc);
        if (terrain){
            mat <- interp(x,y,z,xo=xc,yo=yc);
            H <- mat$z + mdata$z;   # Terrain + spatial data
            vname=paste(zname,'+',name);
        }else{
            H <- mdata$z;
            vname=name;
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
    

    H <-  H;
     Hlim <- range( H[!is.na(H)] )
    factor=1;
    if(Hlim[1]==0 & Hlim[2]==0){
        Hlim[2]=Hlim[1]+1
    }

    while(diff(Hlim)<1 ){
          factor=factor*10;
          Hlim=Hlim*factor;
    }
    Hlen <- round(Hlim[2] - Hlim[1] + 1 )

    colorlut <- terrain.colors(Hlen,alpha=1) # height color lookup table
    col <- colorlut[ H*factor-Hlim[1]+1 ] # assign colors to heights for each point
    rgl.open();
    par3d(windowRect = c(10, 10, 600, 600))
    Sys.sleep(0.1)
#    surface3d(xc, yc, H, coord=1:3, color=col, alpha=0.75, back="lines")
    terrain3d(xc, yc, H, coord=1:3, color=col, alpha=0.75, back="lines")
     #aspect3d(x=1,y=1,z=zr)
    bgplot3d( suppressWarnings ( image.plot( legend.only=TRUE, legend.args=list(text=vname), zlim=Hlim/factor,col=colorlut) )  )#legend

    #persp3d(xc, yc, H, coord=1:3, color=col, alpha=0.75, back="lines")

    grid3d(c("x", "y+", "z"))
    #rgl.surface(yc, xc, H, coord=1:3, color=col, alpha=0.75, back="lines")
    
    colorlut <- heat.colors(Hlen,alpha=1) # use different colors for the contour map
    col <- colorlut[ H-Hlim[1]+1 ] 
    if (dist ==0){
        dist=mean(H)-diff(Hlim);
    }else{
        dist <- dist;
    }
    
    surface3d(xc, yc, matrix(dist, nrow(H), ncol(H)),color=col, back="fill")
    rgl.viewpoint(0, -20)
    
    filepath=file.path(path,fn); 
    nf=nchar(filepath);
    filefmt=substr(filepath,nf-2,nf);
    if(grepl('eps|svg|pdf',filefmt) ){
        rgl.postscript(filepath, fmt = filefmt)
    }else{
        snapshot3d(filepath, fmt = filefmt);
    }
    return(diff(Hlim));
}

PIHM.trimesh<-function(cellid=0){
    mesh <- readmesh(bak=TRUE);
    riv <- readriv(bak=TRUE);
    msh <- mesh$mesh;
    pts <- mesh$points;
    tri <- msh[,2:4];
    m <- nrow(tri);
    x=t(matrix(c(pts[tri[,1],2],pts[tri[,2],2],pts[tri[,3],2]),m,3) );
    y=t(matrix(c(pts[tri[,1],3],pts[tri[,2],3],pts[tri[,3],3]),m,3) );
    z=t(matrix(c(pts[tri[,1],4],pts[tri[,2],4],pts[tri[,3],4]),m,3) );
    
    if(cellid==0){
        cellid=1:m;
    }
    rows=combn(1:3,2);
    xx=x[rows,cellid];
    yy=y[rows,cellid];
    zz=z[rows,cellid];
    segments3d(xx,yy,zz,color=rgb(0,.81,0,alpha=0.5))
    PIHM.rivplot(shift=100);

}

PIHM.triplot<-function(data,cellid,rivid,terrain=FALSE, name='value',zratio=1,fn='triplot.png',path=Resultpath,title='3Dtriangles',color='red',shift=100){
    mesh <- readmesh(bak=TRUE);
    riv <- readriv(bak=TRUE);
    msh <- mesh$mesh;
    pts <- mesh$points;
    tri <- msh[,2:4];
    m <- nrow(tri);
    x=t(matrix(c(pts[tri[,1],2],pts[tri[,2],2],pts[tri[,3],2]),m,3) );
    y=t(matrix(c(pts[tri[,1],3],pts[tri[,2],3],pts[tri[,3],3]),m,3) );
    z=t(matrix(c(pts[tri[,1],4],pts[tri[,2],4],pts[tri[,3],4]),m,3) );
    H=(pts[msh[,2],4]+pts[msh[,3],4]+pts[msh[,4],4] )/3
    #H=t(matrix( (pts[msh[,2],4]+pts[msh[,3],4]+pts[msh[,4],4] )/3, m,3))
    zname='Elevation';

    if (missing(data)){ #missing data = Elevation
        vname=zname;
    }else{
        if(length(data)==length(H)){
            if(terrain){    #data + Terrain.
                H=H+data;
                vname=paste(zname,'+',name);
            }else{
                H=data; #Data only.
                vname=name;
            }
        }else{
            stop('Dimension of data does not match the grid data\n');
        }
    }
    H=t(matrix(rep(H,3),m,3))
     Hlim <- range(H)
    xlim<- range(x);
    ylim<-range(y)
    dhdx <- diff(Hlim) / min(diff(xlim),diff(ylim));
    
    zr <- 1;
    if (dhdx < 1/2000 ) {
        zr <-  round(1/2000/dhdx) ;
       # warning('\n\tdh/dx = ',round(dhdx,digits=5),'.\tZ ratio > ', zr, ' is recommended');
        if (zratio <= 0){
            zr <- zr;
        }else{
            zr <- zratio;
        }
    }
    z=z;
    factor=1;
    Hlim <- range(H)
    if(Hlim[1]==0 & Hlim[2]==0){
        Hlim[2]=Hlim[1]+1
    }
    while(diff(Hlim)<1 ){
          factor=factor*10;
          Hlim=Hlim*factor;
    }
    Hlen <- round(Hlim[2] - Hlim[1] + 1 )

    colorlut <- terrain.colors(Hlen,alpha=1) # height color lookup table
    col <- colorlut[ H*factor-Hlim[1]+1 ] # assign colors to heights for each point
    

    
   # open3d()
    rgl.open();
    par3d(windowRect = c(10, 10, 612, 612))
    Sys.sleep(0.1)
    triangles3d(x,y,z,color=col,box=TRUE)
    bgplot3d( suppressWarnings ( image.plot( legend.only=TRUE, legend.args=list(text=vname), zlim=Hlim/factor,col=colorlut) ) )#legend
    #axis3d('z')
    rgl.viewpoint(0, 20)
    #aspect3d(x=1,y=1,z=zr)


    if(!missing(cellid)){   #highlight cells
        if(!( 0 %in% cellid) ){
            rows=combn(1:3,2);
            xx=x[rows,cellid];
            yy=y[rows,cellid];
            zz=z[rows,cellid];
            segments3d(xx,yy,zz+shift,color=color,lwd=2)
            #segments3d(x[,cellid],y[,cellid],z[,cellid]+10,color=rgb(1,0,0,alpha=0.5),lwd=2)
           # triangles3d(x[,cellid],y[,cellid],z[,cellid]+10,color=rgb(1,0,0,alpha=0.5))
        }
    }
    
    PIHM.rivplot(shift=10,zratio=zr);    #plot river network.

    if(!missing(rivid)){  #highlight the segments of rivid.
        PIHM.rivplot(rivid=rivid,shift=1,zratio=zr); 
    }

    title3d(title, col = 'blue')
    rgl.viewpoint(0, 20)
    
    filepath=file.path(path,fn);
    nf=nchar(filepath);
    filefmt=substr(filepath,nf-2,nf);
    if(grepl('eps|svg|pdf',filefmt) ){
        rgl.postscript(filepath, fmt = filefmt)
    }else{
        snapshot3d(filepath, fmt = filefmt);
    }
    
}

PIHM.rivplot<-function(data,rivid,terrain=FALSE, zratio=0,dist=0,shift=5,color='blue'){
    mesh <- readmesh(bak=TRUE);
    riv <- readriv(bak=TRUE);
    msh <- mesh$mesh;
    pts <- mesh$points;
    #=====plot river segments==========
    seg <- riv$River$riv[,c('FROM','TO')]
    
    m <-nrow(seg)
    x=t(matrix(c(pts[seg[,1],2],pts[seg[,2],2]),m,2) );
    y=t(matrix(c(pts[seg[,1],3],pts[seg[,2],3]),m,2) );
    z=t(matrix((pts[seg[,1],5]+pts[seg[,2],5])/2,m,2) )*zratio;
    
    if(shift <0){
        moveup =t(matrix( abs(diff(z)),m,2));
    }else{
        moveup=shift;
    }
    
    z=z+moveup;
    lwidth <- t(matrix( riv$River$riv[,'SHAPE'] ,m,2))


     Hlim <- range(lwidth)
    if(diff(Hlim)==0){
        Hlim[2]=Hlim[1]+1
    }
    Hlen <- max(round(Hlim[2] - Hlim[1] + 1 ),2)

    Hlen <- Hlim[2] - Hlim[1] + 1
    colorlut <- heat.colors(Hlen,alpha=0.85) # height color lookup table
    col <- colorlut[ lwidth-Hlim[1]+1 ] # assign colors to heights for each point
    
    segments3d(x,y,z,color=col,lwd=1)
    if(!missing(rivid)){
        if (!(rivid==0))
            segments3d(x[,rivid],y[,rivid],z[,rivid]-moveup/2,color=color,lwd=2)
    }

}
