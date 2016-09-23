#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#'

RiverAtt <- function (    riv=readriv(), mesh = readmesh()){
    pt = mesh$points;
    mr = riv$River$riv
    p1 = mr[,2]  #from
    p2 = mr[,3] #to
    
    z1 = pt[p1,5]
    z2 = pt[p2,5]
    xy1 = pt[p1,2:3]
    xy2 = pt[p2,2:3]
    deltaH= z1 - z2;  #from - to
    Length=Eudist(xy1,xy2)  #length;
    slope = deltaH / Length #slope
    
    Shape = riv$Shape$shp[mr[,7],-1]
    Material = riv$Material$mat[mr[,8], -1]

    p.xy= cbind(xy1,xy2)
    colnames(p.xy)= toupper(c('X.From', 'Y.From','X.To','Y.To'))

    x.c = (p.xy[,1] + p.xy[,3])/2   #x of center
    y.c = (p.xy[,2] + p.xy[,4])/2
    z.cbed = (z1 + z2) / 2 -  Shape[,1] # zmax - riverdepth
    PCenter= cbind(x.c, y.c, z.cbed)
    colnames(PCenter) = c('X.c', 'Y.c', 'Z.cbed')

    att = cbind(mr,p.xy, PCenter, deltaH, Length, slope, Shape, Material)
    return(att)
}


SinksRiverRemove <- function( riv=readriv(), mesh = readmesh(), value = 0.01){
    #default change value is 5cm.
#    riv=readriv(); mesh = readmesh(); value = 0.05;
    org = mesh;
    message('\nEach Segements')
    ne = 1
    pt=mesh$points;
    while(1){
        rivdbf=RiverAtt(riv=riv, mesh=mesh);
        ido= rivdbf[,1]; 
        id.fr = rivdbf[ido,2]
        id.to = rivdbf[ido,3]
        zmx.to = pt[id.to,5]    
        zmx.fr = pt[id.fr,5]

        diff.zmx = (zmx.fr - zmx.to) 
        tarID = ido[which( diff.zmx<=0 )] # gradient of segements is negative or zero
        if (length(tarID)>0 ){
            print(tarID) 
            print(diff.zmx[tarID])
            message('#',ne,'\t',length(tarID),' segements are negative slope') 
            ne=ne+1
            pt[id.to[tarID], 4:5] = pt[id.fr[tarID], 4:5]-value # move down stream by 5cm.  
            mesh$points=pt
        }else{
            break;
        }
    }
#    riv=readriv(); mesh = readmesh(); value = 0.05;

    message('\nSegement FROM to DOWN')
    pt = mesh$points;
    ne = 1
    while(1){
        rivdbf=RiverAtt(riv=riv, mesh=mesh);
        id.cr = which(rivdbf[,4] >0)    #id of current segements
        tab= rivdbf[id.cr,]
        n=nrow(tab)
        # Test to Segments
        id.to = rivdbf[id.cr,4]     #id of TO segements
        
        bed.to = rivdbf[id.to,'Z.cbed']
        bed.cr = rivdbf[id.cr,'Z.cbed']
        diff.bed = (bed.cr - bed.to)
        tarID = id.cr[which( diff.bed <=0 )] # gradient to DOWN stream is negative or zero
        
        if (length(tarID)>0 ){
            print(tarID)
            print(diff.bed[tarID])
           message('#',ne,'\t',length(tarID),' segements DOWN-SLOPE are negative')
            ne=ne+1
            downID = rivdbf[tarID, 4]
            p.dn = rivdbf[downID, 'TO']
            p.up = rivdbf[tarID, 'FROM']
            pt[p.dn, 4:5] = pt[p.up, 4:5]-value # move down stream by 5cm. 
            mesh$points=pt
        }else{
            break;
        }
    }

    #    riv=readriv(); mesh = readmesh(); value = 0.05;

    message('\nSegement of outlets')
    ne = 1
    pt = mesh$points;
    while(1){
        rivdbf=RiverAtt(riv=riv, mesh=mesh);
        ido= which(rivdbf[,4] <=0)    #id of outlets
        id.fr = rivdbf[ido,2]
        id.to = rivdbf[ido,3]

        zmx.to = pt[id.to,5]    
        zmx.fr = pt[id.fr,5]

        
        tarID = ido[which( (zmx.fr - zmx.to) <=0 )] # gradient to DOWN stream is negative or zero
        if (length(tarID)>0 ){
            print(tarID)
            message('#',ne,'\t',length(tarID),' outlet-segements are negative slope') 
            ne=ne+1
            pt[id.to, 4:5] = pt[id.fr, 4:5]-value # move down stream by 5cm.  
            mesh$points=pt
        }else{
            break;
        }
    }
    
    df = org$points[,5]- mesh$points[,5]
    dfid = which(df !=0)
    message(length(dfid),'points was modified')
  #  print(df)
    return(mesh)
}

