
readUSGSgageLocal <- function (fn) {
    lines <- readLines(fn);
    lines <- lines[!is.na(lines)];
    slongname =as.character(substr(lines[15],4,nchar(lines[15]) ) );
    
    rid <- which(grepl('^agency_cd',tolower(lines)))+2;

    mat <- as.matrix(read.table(text=lines[rid:length(lines)]));
    sname <- unique(mat[,2]);
    t <- as.Date(mat[,3]) ;
    ts <- xts(as.numeric(mat[,4])*0.3048^3,order.by=t)
    colnames(ts) <- paste(sname,'m^3/s') ;
    return( ts );
}

readUSGSQ <- function(siteid,sdate,edate){
    loadinglib(liblist=c('EGRET' , 'dataRetrieval') )
    
   # siteid <- '01576754'
    if(missing(sdate)){
        sdate <- '1900-01-01';
    }
    if(missing(edate) ){
        edate <- Sys.Date() ;
    }
    daily <- as.matrix(readNWISDaily(siteNumber=siteid,parameterCd='00060', startDate=sdate, endDate=edate));
    t <- as.Date(daily[,'Date']);
    ts <- xts(as.numeric(daily[,'Q']),order.by=t);
    
}
