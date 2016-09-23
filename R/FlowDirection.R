GradientTri<-function(data,bak=FALSE, elv=0){
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
    zbc     =rowMeans(zbedv);
    zsc     =rowMeans(zsurfv);
    if(missing('data')){    #default = surface elevation.
        Hv   = zsc;
    } else{
        if(elv>0){
            Hv   = zsc + data;  # data + bed elevation, i.e. gw, gu.
        }else if(elv <0){
            Hv   = zbc + data;  #data + surface elevation, i.e. surface storage.
        } else{
            Hv   = data;
        }
    }
    

    hddata = c( paste('Dist',1:3),
            paste('dH',1:3),
            paste('Slope',1:3),
            paste('FlowTo',1:3),        #Flow to this nabor=1, flow from this nabor=0;
            paste('Sinks?')             #sink=1; not sink=0;
            )

    mat<- matrix(0,mesh$size[1],length(hddata));  #data of dataace

    for (i in 1:3){
        nid=nabr[,i]            #nabor's id
        id=which(nid>0);        #id with nabor
        xid=which(nid==0);      #no nabor id.
        
        ptc=cbind(xc[id],yc[id]);           #(x,y) of centroid.
        ptnb=cbind(xc[nid],yc[nid]);  #(x,y) of nabor.

    
        #dataace;
        Hvc=Hv[id];      #elevation of dataace of center.
        Hvnb=Hv[nid]  #elevation of dataace of nabor
        mat[id,i]  =Eudist(ptc,ptnb);      #distance;
        mat[id,i+3]=Hvnb-Hvc ;             # difference of elevation at centroimat.
        tmp= (mat[id,i+3]<=sinkfilter)
        mat[id,i+9]= nid[id] * tmp
        
        mat[id,i+3]=abs(mat[id,i+3]);     # get absolute value.  
        mat[id,i+6]=mat[id,i+3]/mat[id,i] #H/Dist

    }
    
    rsid <- nextrivid(bak=bak);     #index of cell which next to river.
    
    mat[,13]= (rowSums(mat[,10:12])<=0);
    mat[rsid,13]=0;      #remove the cells which are next to rivers.
    colnames(mat)=hddata;


    return(mat);
}

FlowDirection <- function (data, bak=FALSE,elv=0){
    if(missing('data')){    #default = surface elevation.
        mat = GradientTri(bak=bak,elv=elv)
    } else{ 
        mat = GradientTri(data=data,bak=bak,elv=elv)
    }
    
}
