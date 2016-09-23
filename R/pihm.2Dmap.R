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

PIHM.trimesh<-function(cellid=0, riveron=RIVERON){
    mesh <- readmesh(shp=FALSE,bak=TRUE);
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
    if( riveron){
        PIHM.rivplot(shift=100);
    }

}

imagerev<-function(br, left=TRUE) {
    m<-length(br)
    n<-m-1
    A<-diag(n)
    if(left) {
        A[1,1:2]<-c(1.5,-.5)
        A[cbind(rep(2:n,each=2), as.vector(t(embed(1:n,2))))]<-.5
        br<-br[1:n]
    } else {
        A[n,(n-1):n]<-c(-.5,1.5)
        A[cbind(rep(1:(n-1),each=2), as.vector(t(embed(1:n,2))))]<-.5
        br<-br[2:m]
    }
    solve(A,br)
}
PIHM.mapraster<-function(data=getelev(),mask=PIHM.mask(), terrain=FALSE,
                         name=deparse(substitute(data))
                         ,zratio=0,dist=0,fn=paste0(name,'2Dmap.png'),path=Resultpath,
                         title=name,colorFUN=terrain.colors,if.save=FALSE,
                         Hlim, colorreverse=FALSE , ngrids=200,riveron=RIVERON,addcontour=FALSE){
    require(lattice)

    mesh <- readmesh(shp=FALSE,bak=TRUE);
    msh <- mesh$mesh;
    pts <- mesh$points;
    x=(pts[msh[,2],2]+pts[msh[,3],2]+pts[msh[,4],2])/3;
    y=(pts[msh[,2],3]+pts[msh[,3],3]+pts[msh[,4],3])/3;
    z=(pts[msh[,2],4]+pts[msh[,3],4]+pts[msh[,4],4])/3;
    xlim <- range(x);
    ylim <- range(y);
    dx=(xlim[2]-xlim[1])/ngrids;
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
    if(missing(Hlim)){
        Hlim <- round(range(H,na.rm=T))
    }   

    H <-  H;
 
    Hlen=max(diff(round(Hlim),na.rm=T),10);
    Hlen=min(Hlen, 20)
    idd <- classify(seq(from=Hlim[1],to=Hlim[2], length.out=Hlen), H);
    if (colorreverse){
        colorlut <- rev(colorFUN(Hlen,alpha=1))        
    }else{
        colorlut <- colorFUN(Hlen,alpha=1);        
    }
    col <- t( matrix(colorlut[ idd ], nrow=ncol(H))) # assign colors to heights for each point  
   xr = diff(range(x),na.rm=TRUE)
   yr = diff(range(y),na.rm=TRUE)
   if(if.save){
      image.control(path=path,fn=fn, wd=25, ht= 25*yr/xr)
   }
   # levelplot( H , contour =contour)
   image.plot(xc,yc, H , asp=1, col=colorFUN(10), zlim=Hlim)
   if (addcontour){
       contour(xc,yc, H , add=TRUE)
   }

#     colorkey=list(at=seq(0, 1, 0.2), labels=list(at=c(0, 0.3, 0.6, 0.9), labels=c("none", "a bit", "a bit more", "a lot"))
    title(title, col = 'blue')
    if(    riveron){
        riv <- readriv(bak=TRUE);
        PIHM.rivplot();    #plot river network.
    }
   if(if.save){
    dev.off()
   }
    return(H)
}
PIHM.triplot<-function(data=getelev(),cellid,rivid,terrain=FALSE,
                         name=deparse(substitute(data)),
                         fn=paste0(deparse(substitute(data)), '.triplot.png',collapse='')
                         ,path=Resultpath,title='3Dtriangles',color='red',shift=1,colorFUN=terrain.colors,
                         riveron=RIVERON,
                         if.save=TRUE){
        require(lattice)

    mesh <- readmesh(shp=FALSE,bak=TRUE);
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
 xlim=range(x)
 ylim=range(y)
    if (missing(data)){ #missing data = Elevation
        vname=zname;
    }else{
        if(length(data)==length(H)){
            if(terrain){    #data + Terrain.
                H=H+as.numeric(data);
                vname=paste(zname,'+',name);
            }else{
                H=as.numeric(data ); #Data only.
                vname=name;
            }
        }else{
            stop('Dimension of data does not match the grid data\n');
        }
    }
    xlim<- range(x, na.rm=TRUE);
    ylim<-range(y, na.rm=TRUE)

    Hlim <- range(H, na.rm=TRUE)
    Hlim[1] = floor(Hlim[1])
    Hlim[2] = ceiling(Hlim[2]) 
    if (Hlim[1] >0){
        Hlim[1] = round(Hlim[1], -log10(Hlim[1])+1)
    }

    if (Hlim[2] >0){
        Hlim[2] = round(Hlim[2], -log10(Hlim[2])+1)
    } 
    
    Hlen=max(diff(round(Hlim)),5);
    idd <- classify(seq(from=Hlim[1],to=Hlim[2], length.out=Hlen), H );
    colorlut <- colorFUN(Hlen,alpha=1);        
    col <- colorlut[ idd ] # assign colors to heights for each point
   # open3d()
     image.control(fn=fn,path=path, wd=30,ht=25, if.save=if.save)
 plot(0,type='n', asp=1,xlim=xlim,ylim=ylim, ylab='North', xlab='East')
 grid()
 for( i in 1:m){
     polygon(x[,i],y[,i], col=col[i], border='grey')
 }
    if(sd(H, na.rm=TRUE)>1e-6){
         suppressWarnings ( image.plot( legend.only=TRUE, legend.args=list(text=vname), zlim=Hlim,col=colorlut) )
    }else{
         suppressWarnings ( image.plot( legend.only=TRUE, legend.args=list(text=vname), zlim=c(Hlim[1],Hlim[2]+0.1),col=colorlut) )#legend
    }

message('debug')
    if(!missing(cellid)){   #highlight cells
        if(!( 0 %in% cellid) ){
            rows=combn(1:3,2);
            xx=x[rows,cellid];
            yy=y[rows,cellid];
            zz=z[rows,cellid];
           for (k in seq(1,5,by=2) ){ 
               if (length(cellid) >1){
                segments(xx[k,],yy[k,],xx[k+1,],yy[k+1,],col=color)
               }else{
                segments(xx[k],yy[k],xx[k+1],yy[k+1],col=color)
               }
           }
            #segments3d(x[,cellid],y[,cellid],z[,cellid]+10,color=rgb(1,0,0,alpha=0.5),lwd=2)
           # triangles3d(x[,cellid],y[,cellid],z[,cellid]+10,color=rgb(1,0,0,alpha=0.5))
        }
    }
 
    if(riveron){
        PIHM.rivplot();    #plot river network.
        if(!missing(rivid)){  #highlight the segments of rivid.
            PIHM.rivplot(rivid=rivid, color=color); 
        }
    }
    image.off(if.save=if.save)
    return(Hlim)
}

PIHM.rivplot<-function(data,rivid,color='blue'){

    mesh <- readmesh(shp=FALSE,bak=TRUE);
    riv <- readriv(bak=TRUE);
    msh <- mesh$mesh;
    pts <- mesh$points;
    #=====plot river segments==========
    seg <- riv$River$riv[,c('FROM','TO')]
    

    m <-nrow(seg)
    x=t(matrix(c(pts[seg[,1],2],pts[seg[,2],2]),m,2) );
    y=t(matrix(c(pts[seg[,1],3],pts[seg[,2],3]),m,2) );
     
    rivord=t(matrix(riv$River$riv[,7],m,2));
    Hlim <- range(rivord)
    Hlen= diff(range(rivord))+3 ;
    
    colorlut <- heat.colors(Hlen,alpha=1) # height color lookup table
    col <-  colorlut[ rivord+Hlim[1]+1 ] # assign colors to heights for each point 
    
    
    
    segments(x[1,],y[1,],x[2,],y[2,],col=col,lwd=2)
    
    if(!missing(rivid)){
        if (!(rivid==0))
                segments(x[1,rivid],y[1,rivid],x[2,rivid],y[2,rivid],col=color,lwd=2)
    }

}
