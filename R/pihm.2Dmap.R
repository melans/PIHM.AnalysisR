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

PIHM.triplot<-function(data,cellid,rivid,terrain=FALSE, name='value',fn='triplot.png',path=Resultpath,title='3Dtriangles',color='red',shift=1,colorFUN=terrain.colors){
    if ( !x11.ready() ){
        return(0)
    }
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
    z=z * 0;
    Hlim <- range(H)
    Hlen=max(diff(round(Hlim)),5);
    idd <- classify(seq(from=Hlim[1],to=Hlim[2], length.out=Hlen), H);
    colorlut <- colorFUN(Hlen,alpha=1);        
    col <- colorlut[ idd ] # assign colors to heights for each point
    

    
   # open3d()
    rgl.open();
    par3d(windowRect = c(10, 10, 612, 612))
    Sys.sleep(0.1)
    rgl.pop("lights") 
    light3d(specular="black") 
    triangles3d(x,y,z,color=col,box=TRUE) 
    if(sd(H)>1e-6){
        bgplot3d( suppressWarnings ( image.plot( legend.only=TRUE, legend.args=list(text=vname), zlim=Hlim,col=colorlut) ) )#legend
    }else{
        bgplot3d( suppressWarnings ( image.plot( legend.only=TRUE, legend.args=list(text=vname), zlim=c(Hlim[1],Hlim[2]+0.1),col=colorlut) ) )#legend
    }

    #axis3d('z')
    rgl.viewpoint(0)
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
    
    PIHM.rivplot(shift=shift);    #plot river network.

    if(!missing(rivid)){  #highlight the segments of rivid.
        PIHM.rivplot(rivid=rivid,shift=shift+2); 
    }

    title3d(title, col = 'blue')
    #light3d(theta=90,phi=90);
    rgl.viewpoint(0, 0)
    
    filepath=file.path(path,fn);
    nf=nchar(filepath);
    filefmt=substr(filepath,nf-2,nf);
    if(grepl('eps|svg|pdf',filefmt) ){
        rgl.postscript(filepath, fmt = filefmt)
    }else{
        snapshot3d(filepath, fmt = filefmt);
    }
    
}

PIHM.rivplot<-function(data,rivid,terrain=FALSE,dist=0,shift=1,color='blue',riv.ele){
    mesh <- readmesh(bak=TRUE);
    riv <- readriv(bak=TRUE);
    msh <- mesh$mesh;
    pts <- mesh$points;
    #=====plot river segments==========
    seg <- riv$River$riv[,c('FROM','TO')]
    
    m <-nrow(seg)
    x=t(matrix(c(pts[seg[,1],2],pts[seg[,2],2]),m,2) );
    y=t(matrix(c(pts[seg[,1],3],pts[seg[,2],3]),m,2) );
    if(missing(riv.ele)){
        z=t(matrix((pts[seg[,1],5]+pts[seg[,2],5])/2,m,2) );
    }else{
        z=matrix(riv.ele,m,2);
    }
    if(shift <0){
        moveup =t(matrix( abs(diff(z)),m,2));
    }else{
        moveup=shift;
    }
    z=z*0; 
    z=z+moveup;
    rivord=t(matrix(riv$River$riv[,7],m,2));
    
   # lwidth <- t(matrix( riv$River$riv[,'SHAPE'] ,m,2))


     Hlim <- range(rivord)
    Hlen= diff(range(rivord))+3 ;
    
    colorlut <- heat.colors(Hlen,alpha=1) # height color lookup table
    col <-  colorlut[ rivord+Hlim[1]+1 ] # assign colors to heights for each point 
    
    segments3d(x,y,z,color=col,lwd=2)
    if(!missing(rivid)){
        if (!(rivid==0))
            segments3d(x[,rivid],y[,rivid],z[,rivid]+moveup/2,color=color,lwd=2)
    }

}

