#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;

readASC <- function ( fn, file, path='./'){
    if(missing(file)){
        file=file.path(path,fn)
    }
    #file='/Users/leleshu/Documents/PHIM_PRJ/Mendota/Gssurgo/Processed/landcover.asc'
    
    txt = readLines(file)
    hids=which(grepl('^[[:alpha:]]', txt[1:20]))
    h=as.matrix(read.table(text = txt[hids],row.names=1), col=1)       # header
    data=as.matrix(read.table(text = txt[-(hids)]))
    x=h['xllcorner',1] + (0:(h['ncols',1]-1))*h['cellsize',1];
    y=h['yllcorner',1]+(0:(h['nrows',1]-1))*h['cellsize',1];
    z=data;
    ret=list('header'=h, 'x'=x, 'y'=y,'z'=z);
    return(ret);
}
writeASC <- function(data,path=outpath, fn='map.asc', file, nodata = -9999){
    if(missing(file)){
        file=file.path(path,fn)
    }
    x=data$x
    y=data$y
    z=data$z
    ncols = length(y)
    nrows = length(x)

    xllcorner = min(x, na.rm=TRUE)
    yllcorner = min(y, na.rm=TRUE)
    cellsize = x[2]-x[1]

    zo =t(z)

    z=zo[rev(1:ncols),] 

    NODATA_value = nodata 

    z[which(is.na(z))]= NODATA_value
    write(file=fn,paste('ncols',nrows),append=FALSE)    # note. ncols = nrows
    write(file=fn,paste('nrows',ncols),append=TRUE)     #note nrows = ncols
    write(file=fn,paste('xllcorner',xllcorner),append=TRUE)
    write(file=fn,paste('yllcorner',yllcorner),append=TRUE)
    write(file=fn,paste('cellsize',cellsize),append=TRUE)
    write(file=fn,paste('NODATA_value',NODATA_value),append=TRUE)
    write.table(z,file=fn,row.names=F, col.names=F, append=TRUE, quote=TRUE)

#
#    write.table(data$header, file=file,row.names=T, col.names=F, append=FALSE,quote=FALSE)
#    write.table(data$z,file=file,row.names=F, col.names=F, append=TRUE, quote=FALSE)
}

ASC.statistics <- function (data,path=outpath, fn,file){
    if(missing(data)){
        if(missing(file)){
            file=file.path(path,fn)
            data=readASC(file=file)
        }
    }
    z=as.numeric(data$z);
    z[which(z == data$header[7])]=NA
    a = data$header[5] *  data$header[6]
    Area = length(which(!is.na(z))) * a
    
    uid = sort(unique(z[which(!is.na(z)) ]))
    nid = length(uid)
    tab = matrix(0, nrow = nid, ncol =3)
    tab[,1]= uid
    colnames(tab) = c('NLCD CODE','Area','Percentage');
    for (i in 1:nid){
        tab[i,2]= length(which(z == tab[i,1] )) * a  
        tab[i,3]= tab[i,2]/Area * 100 ;
    }
    ret = list('unique'=uid,
               'table'=tab,
               'rang' = range(uid))
    return(ret)
}

MeshData2ASC <- function(data, if.ele=0,ngrids=200,resolution, path=Resultpath, fn='MeshData2ASC.asc', file){
    if(missing(file)){
        file=file.path(path,fn)
    }
#Interpolate and write ASC map out.
    tmp = InterpSpatial(data=data, elevation=if.ele, resolution=resolution)
    writeASC(data = tmp, fn=file)
    return(tmp)

}


shp2raster <- function (shp, mask, resolution=0, field=1) {
library(raster)
## Set up a raster "template" to use in rasterize()
      if( resolution <=0 ){
          resolution = round(max(diff(ext[1:2]), diff(ext[3:4]))/100, -1)
            message('Resolution is ', resolution);
      }
if(missing('mask')){
    ext <-  extent (shp)
    xy <- abs(apply(as.matrix(bbox(ext)), 1, diff))
    r <- raster(ext, res=resolution)
}else{
    r = mask
}
## Rasterize the shapefile
rr <-rasterize(shp, r, field=field)
return(rr)
}

PIHM.mask  <- function (mesh=readmesh(),  resolution=0){
    Shp.mesh = mesh$shp;
    ext = extent(Shp.mesh)
      if( resolution <=0 ){
          resolution = round(max(diff(ext[1:2]), diff(ext[3:4]))/100, -1)
      }
      message('Resolution is ', resolution);
      r=raster(ext=ext, res=resolution)
      mask = rasterize(Shp.mesh, r, field='MACP')
      fun <- function(x) { x[!is.na(x) ] <- NA; return(x) }
      mask <- calc(mask, fun)
      #assign("r.mask",mask , envir = .GlobalEnv)
      return(mask)
}


Riv2Shp <- function(  riv=readriv(), mesh = readmesh(),
                    path=file.path(ModelInfopath,'GISlayer')){
    rivdbf=RiverAtt(riv=riv, mesh=mesh);
    n=nrow(rivdbf)
    xy1 = rivdbf[,c('X.FROM', 'Y.FROM')]
    xy2 = rivdbf[,c('X.TO', 'Y.TO')]
    llist = list()
    sl = list()
    for(i in 1:n){
        llist[[i]] = Line(rbind( xy1[i,], xy2[i,]) )
        sl[[i]] = Lines(llist[[i]], ID=paste0(i))
    }
    spl = SpatialLines(sl)
    sp.riv = SpatialLinesDataFrame(spl, as.data.frame(rivdbf))
    return(sp.riv)
    
}


