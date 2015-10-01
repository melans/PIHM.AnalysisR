#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 10:53:00 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;
#' 
#' 
#'  <- ============================================
#' @param  Path of output folder.
#' @param  prpjectname.
#' @keywords read input. mesh file.
#' @export  List of river data. river
#' @examples
#' readriv()

checksinks <- function(bak=TRUE){
    mat<-check.meshsinks(bak=bak);
    sinkid<- which(mat[,'Sinks_surf?']>0);
    PIHM.triplot(cellid=sinkid,fn='Sinks_Surf.png',title='Surface Sinks');
    
    bedid<- which(mat[,'Sinks_bed?']>0);
    if( ! identical(sinkid,bedid) ){
        PIHM.triplot(cellid=bedid,fn='Sinks_bed.png',title='Surface Sinks');
    }   

    sbid<-which(mat[,'UpperBed?'] >0);
    PIHM.triplot(cellid=sbid,fn='Sinks_HigherNaborBed.png',title='cells whose nabor\'s bed is higher than its surface');
   
    return(list('sinkid'=sinkid,'highNaborBedid'=sbid));

}

getelev <- function(bak=TRUE){
    mesh <- readmesh(bak=bak);
    msh <- mesh$mesh;
    pts <- mesh$points;
    nabr<-mesh$mesh[,5:7]   #nabor or each cell.
    node<-mesh$mesh[,2:4]    #vetex of each cell.
    
    xv       =cbind(pts[node[,1],2],pts[node[,2],2],pts[node[,3],2]); #end with v, means vertex of triangles.
    yv       =cbind(pts[node[,1],3],pts[node[,2],3],pts[node[,3],3]);
    zbedv    =cbind(pts[node[,1],4],pts[node[,2],4],pts[node[,3],4]);
    zsurfv   =cbind(pts[node[,1],5],pts[node[,2],5],pts[node[,3],5]);
    
    xc      =rowMeans(xv);      #end of c, means centronid of the triangles.
    yc      =rowMeans(yv);
    zbedc   =rowMeans(zbedv);
    zsurfc  =rowMeans(zsurfv);
    return(zsurfc);
    
}
check.meshsinks <- function(bak=TRUE,sinkfilter=0){
    mesh <- readmesh(bak=bak);
    msh <- mesh$mesh;
    pts <- mesh$points;
    nabr<-mesh$mesh[,5:7]   #nabor or each cell.
    node<-mesh$mesh[,2:4]    #vetex of each cell.
    
    xv       =cbind(pts[node[,1],2],pts[node[,2],2],pts[node[,3],2]); #end with v, means vertex of triangles.
    yv       =cbind(pts[node[,1],3],pts[node[,2],3],pts[node[,3],3]);
    zbedv    =cbind(pts[node[,1],4],pts[node[,2],4],pts[node[,3],4]);
    zsurfv   =cbind(pts[node[,1],5],pts[node[,2],5],pts[node[,3],5]);
    
    xc      =rowMeans(xv);      #end of c, means centronid of the triangles.
    yc      =rowMeans(yv);
    zbedc   =rowMeans(zbedv);
    zsurfc  =rowMeans(zsurfv);
    hdsurf = c( paste('Dist',1:3),
            paste('dH_surf',1:3),
            paste('Slop_surf',1:3),
            paste('FowTo_surf',1:3),        #Flow to this nabor=1, flow from this nabor=0;
            paste('Sinks_surf?')             #sink=1; not sink=0;
            )
           
    hdbed = c( paste('Dist',1:3), 
            paste('dH_bed',1:3),
            paste('Slop_bed',1:3),
            paste('FowTo_bed',1:3),
            paste('Sinks_bed?')
             )
    hdsb =c(paste('Csurf-Nbed',1:3),
            'UpperBed?'
            )
    ds<- matrix(0,mesh$size[1],length(hdsurf));  #data of surface
    db<- matrix(0,mesh$size[1],length(hdbed));  #data of bed
    sb<- matrix(0,mesh$size[1],length(hdsb));  #data of bed

    for (i in 1:3){
        nid=nabr[,i]            #nabor's id
        id=which(nid>0);        #id with nabor
        xid=which(nid==0);      #no nabor id.
        
        ptc=cbind(xc[id],yc[id]);           #(x,y) of centroid.
        ptnb=cbind(xc[nid],yc[nid]);  #(x,y) of nabor.

    
        #surface;
        Hsc=zsurfc[id];      #elevation of surface of center.
        Hsnb=zsurfc[nid]  #elevation of surface of nabor
        ds[id,i]  =Eudist(ptc,ptnb);      #distance;
        ds[id,i+3]=Hsnb-Hsc ;             # difference of elevation at centroids.
        ds[id,i+9]= (ds[id,i+3]<=sinkfilter)
        
        ds[id,i+3]=abs(ds[id,i+3]);     # get absolute value.  
        ds[id,i+6]=ds[id,i+3]/ds[id,i] #H/Dist

        #bed=======
        Hbc=zbedc[id];      #elevation of bed of center.
        Hbnb=zbedc[nid]  #elevation of bed of nabor
        db[id,i]  =Eudist(ptc,ptnb);      #distance;
        db[id,i+3]=Hbnb-Hbc ;             # difference of elevation at centroids.
        db[id,i+9]= (db[id,i+3]<0)
        
        db[id,i+3]=abs(db[id,i+3]);     # get absolute value.
        db[id,i+6]=db[id,i+3]/db[id,i] #H/Dist

        sb[id,i]=Hsc-Hbnb;      #surface elveation of cendtoir - bed elevation of nabor. if <0, nabor is too much higher than me.
        sb[id,4]= sb[id,i]<0
    }
    
    rsid <- nextrivid(bak=bak);     #index of cell which next to river.
    
    ds[,13]= (rowSums(ds[,10:12])<=0);
    ds[rsid,13]=0;      #remove the cells which are next to rivers.
    colnames(ds)=hdsurf;

    
    db[,13]= (rowSums(db[,10:12])<=0);
    db[rsid,13]=0;      #remove the cells which are next to rivers.
    colnames(db)=hdbed;
    
    colnames(sb) = hdsb; 

    ret=cbind(ds,db,sb);
    return(ret);
}


soildepth <- function(bak=TRUE){ 
    mesh <- readmesh(bak=bak);
    msh <- mesh$mesh;
    pts <- mesh$points;

    nabr<-mesh$mesh[,5:7]   #nabor or each cell.
    node<-mesh$mesh[,2:4]    #vetex of each cell.

    
    xv       =cbind(pts[node[,1],2],pts[node[,2],2],pts[node[,3],2]); #end with v, means vertex of triangles.
    yv       =cbind(pts[node[,1],3],pts[node[,2],3],pts[node[,3],3]);
    zbedv    =cbind(pts[node[,1],4],pts[node[,2],4],pts[node[,3],4]);
    zsurfv   =cbind(pts[node[,1],5],pts[node[,2],5],pts[node[,3],5]);

    
    sd= rowMeans(zsurfv)-rowMeans(zbedv);
    return(sd);
}

nextrivid <-function (bak=TRUE){
    riv<-readriv(bak=bak)$River$riv;
    allid<-riv[,c('LEFT','RIGHT')];
    ids<- unique(as.numeric(allid) );
    return(ids);
}
