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
 
PIHM.3Dmap<-function(data= getelev(),terrain=FALSE,name=deparse(substitute(data)),
                     zratio=0,dist=0,
                     fn=paste0(deparse(substitute(data)), '.3Dmap.png'),path=Resultpath,title=name
                     ,heatmap=FALSE,colorFUN=terrain.colors,
                     Hlim, colorreverse=FALSE , ngrids=200,riveron=RIVERON){
    loadinglib(liblist=c('rgl'))
    
    mesh <- readmesh(bak=TRUE);
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
    

    H <-  H * zr;
    Hlim=Hlim * zr;     #enlarge
    
    Hlen=max(diff(round(Hlim)),10);
    idd <- classify(seq(from=Hlim[1],to=Hlim[2], length.out=Hlen), H);
    Hlim=Hlim / zr;     #Go back.
    
    if (colorreverse){
        colorlut <- rev(colorFUN(Hlen,alpha=1))        
    }else{
        colorlut <- colorFUN(Hlen,alpha=1);        
    }
    col <- colorlut[ idd ] # assign colors to heights for each point  
    
    rgl.open();
    par3d(windowRect = c(10, 10, 600, 600))
    Sys.sleep(0.1)
    rgl.pop("lights") 
    light3d(specular="black") 
#    surface3d(xc, yc, H, coord=1:3, color=col, alpha=0.75, back="lines")
    terrain3d(xc, yc, H, coord=1:3, color=col, alpha=0.75, back="lines")
     #aspect3d(x=1,y=1,z=zr)
    if(sd(H,na.rm=TRUE)>1e-6){
        message("stand deviation=",sd(H,na.rm=TRUE));
        bgplot3d( suppressWarnings ( image.plot( legend.only=TRUE, legend.args=list(text=vname), zlim=Hlim,col=colorlut) )  )#legend
    }
    #persp3d(xc, yc, H, coord=1:3, color=col, alpha=0.75, back="lines")

    grid3d(c("x", "y+", "z"))
    #rgl.surface(yc, xc, H, coord=1:3, color=col, alpha=0.75, back="lines")
    if (heatmap){
        colorlut <- heat.colors(Hlen,alpha=1) # use different colors for the contour map
        col <- colorlut[ idd] 
        if (dist ==0){
            dist=mean(H[!is.na(H)])-diff(Hlim);
        }else{
            dist <- dist;
        }
        
        surface3d(xc, yc, matrix(dist, nrow(H), ncol(H)),color=col, back="fill")
    }
    
    title3d(title, col = 'blue')
    if(    riveron){
        riv <- readriv(bak=TRUE);
        PIHM.3Drivplot(shift=dist,zr=zr);    #plot river network.
    }

    rgl.viewpoint(0, 0)
    
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

PIHM.3Dtriplot<-function(data=getelev(),cellid,rivid,terrain=FALSE,
                       name=deparse(substitute(data)),
                       fn=paste0(deparse(substitute(data)), '.triplot.png'),path=Resultpath,
                       title=deparse(substitute(data)),
                       color='red',shift=1,colorFUN=terrain.colors,colormap,
                       colorreverse=FALSE,Hlim, riveron=RIVERON,zr=1){
    loadinglib(liblist=c('rgl'))
    
mesh <- readmesh(bak=TRUE,shp=FALSE);
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
    H=as.numeric(H)
    H=rbind(H, rbind(H,H))
    if(missing(Hlim)){
     Hlim <- range(H)
    }
    xlim<- range(x);
    ylim<-range(y)
    dhdx <- diff(Hlim) / min(diff(xlim),diff(ylim));
    z=z * zr;
    Hlen=max(diff(round(Hlim)),10);
    idd <- classify(seq(from=Hlim[1],to=Hlim[2], length.out=Hlen), H);
    if (colorreverse){
        colorlut <- rev(colorFUN(Hlen,alpha=1))        
    }else{
        colorlut <- colorFUN(Hlen,alpha=1);        
    }
    col <- matrix(colorlut[ idd ], nrow=3) # assign colors to heights for each point
    
    if (!missing(colormap)){
        col=matrix(colormap[H], nrow=3)
        idvalue=sort(unique(data))
        colorlut=unique(colormap[idvalue])
        idvalue=c(idvalue,100)
    }
    
   # open3d()
    rgl.open();
    par3d(windowRect = c(10, 10, 612, 612))
    Sys.sleep(0.1)
    rgl.pop("lights") 
    light3d(specular="black") 
    triangles3d(x,y,z,color=col,box=TRUE) 

    if (!missing(colormap)){
                
        #===========================
        #reference http://stackoverflow.com/questions/23993864/deviation-of-axis-and-breaks-in-image-plot-from-the-fields-package    
        image.plot2<-image.plot
        body(image.plot2)[[17]]<-quote(iy<-imagerev(breaks))
        # For Landcover map legend bar.
        #===========================

        breaks=1:length(idvalue)
        bgplot3d( suppressWarnings (image.plot2( legend.only=TRUE,breaks=breaks,lab.breaks=idvalue,col=colorlut, zlim=c(breaks) ) ) )#legend
    }else{
        if(sd(H)>1e-6){
            bgplot3d( suppressWarnings ( image.plot( legend.only=TRUE, legend.args=list(text=vname), zlim=Hlim,col=colorlut) ) )#legend
        }else{
            bgplot3d( suppressWarnings ( image.plot( legend.only=TRUE, legend.args=list(text=vname), zlim=c(Hlim[1],Hlim[2]+0.1),col=colorlut) ) )#legend
        }
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
    if(    riveron){
        riv <- readriv(bak=TRUE);
        PIHM.3Drivplot(shift=shift,zr=zr);    #plot river network.
    }
    if(!missing(rivid)){  #highlight the segments of rivid.
        PIHM.3Drivplot(rivid=rivid,shift=shift+2, zr=zr); 
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

PIHM.3Drivplot<-function(data,rivid,terrain=FALSE,dist=0,shift=1,color='blue',riv.ele,zr=1){
    loadinglib(liblist=c('rgl'))
    
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
    
    z=z*zr; 
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


PIHM.3Dclose <- function(){
    loadinglib(liblist=c('rgl'))
    
    l <- rgl.dev.list();
    nl= length(l);
    if ( nl>0 ) {
        for (i in 1:nl ){
            rgl.close();
        }
    }
    return(nl);
}
