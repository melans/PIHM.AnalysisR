#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;

Matrix2ASC <- function(data, fn='map.asc', nodata=-9999){
    
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
write.table(z,file=fn,row.names=F, col.names=F, append=TRUE, quote=FALSE)

}

readASC <- function (path='./', fn,file){
    if(missing(file)){
        file=file.path(path,fn)
    }
    #file='/Users/leleshu/Documents/PHIM_PRJ/Mendota/Gssurgo/Processed/landcover.asc'
    
    txt = readLines(file)
    h=as.matrix(read.table(text = txt[1:6],row.names=1), col=1)       # header
    data=as.matrix(read.table(text = txt[-(1:6)]))
    x=h[3]+(0:(h[1]-1))*h[5];
    y=h[4]+(0:(h[2]-1))*h[5];
    z=data;
    ret=list('header'=h, 'x'=x, 'y'=y,'z'=z);
    return(ret);
}
writeASC <- function(data,path='./', fn='map.asc', file){
    if(missing(file)){
        file=file.path(path,fn)
    }
    write.table(data$header, file=file,row.names=T, col.names=F, append=FALSE,quote=FALSE)
    write.table(data$z,file=file,row.names=F, col.names=F, append=TRUE, quote=FALSE)
}

