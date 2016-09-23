
read.grib <- function (fn,
                   variables=c('APCP','SPFH','TMP','UGRD','VGRD','DLWRF','DSWRF','PRES'),
                   na.thrd = 1e10, tmpfile= 'tmp.txt') {
        op <- options("warn")
        options(warn = -1)
        test <- tryCatch(system("wgrib", intern = TRUE,ignore.stderr=T, ignore.stdout=T))
        if (attr(test, "status") != 8) {
            stop("wgrib does not appear to be installed, or it is not on the PATH variable.\n                  You can find wgrib here: http://www.cpc.ncep.noaa.gov/products/wesley/wgrib.html.\n                  It is also available as an Ubuntu package.")
        }
        key =paste(variables, collapse=':|:')
        
        m = 464;
        n = 224;
     #   wg.str <- paste0("wgrib -s ", fn, " | grep \":", paste(key, collapse=':|:'), ":\" | wgrib -V -i -text -nh ", fn, " -o tmp.txt");
        wg.str <- paste( "wgrib -v ", fn, " | egrep \"(:", paste(key, collapse=':|:'), ":)\" | wgrib -i -text -nh ",fn, " -o ",tmpfile, sep='') 
                         wg.str
        metadat <- system(wg.str, intern=TRUE, ignore.stderr = TRUE)
        value<- scan(tmpfile,  quiet = TRUE)
        value[which (value > na.thrd) ]=NA
        dnames = as.matrix( read.table(text=metadat,  sep=':') )[,4]
        mat = array(value, c(m,n,length(value)/m/n),
                    dimnames = list(paste0('x',1:m),
                            paste0('y',1:n),
                            dnames) ) 

        nc= nchar(fn)
        str = substr(fn, nc - 20, nc-8)
        t= as.POSIXlt(str,format = '%Y%m%d.%H%M', tz='GMT')

    data = list('data'=mat, 'time'=t, 'metadata'=metadat)
    return(data)
}

NLDAS2LocDeg <-function(lxy){
    
    o=rep.row(c(-125, 25) + c(1/16,1/16), max(dim(lxy)) ) # start point
    str = paste0('x',lxy[,1], 'y',lxy[,2])
    ret = o + lxy*1/8;
    colnames(ret)=c('Longitude','Latitude')
    rownames(ret) = str;
    return(ret)
    
}
