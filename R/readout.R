#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' @param  fn Full path of input file. The file has to be in matrix format and first colomn is in "YYYY-MM-DD hh:mm".
#' @keywords read output. Could be used for reading mesh and river format.
#' @export  output data.
#' @return A TimeSeries data. This list require support of xts packages.
#' @examples
#' readout(fn)


readout <-function(fn,binary=FALSE){


    
    nargin <- nargs()
    if (nargin <1 ){
        cat("\nUsage:\n\t data = readout(fn)\n");
        cat("Where:\ndata = [T,data]\n");
        cat("\n\n");
        return(0);
    }
#==============================
readpihmmf <- function(fn,binary){
    if (binary){
        mesh <- readmesh();
        maxn=100*365*24*10000;
        d <- readBin(fn,what=numeric(),n=maxn*mesh$size[1]);

    }else{
        d <- read.table(fn);
        t <- as.Date(d[,1]);
        data <- d[,-1];
        ts <- xts(data,order.by=t);
    }
    return(ts);
}
readpihm20 <- function(fn){
    d <- read.table(fn);
    t <- as.Date(d[,1]);
    data <- d[,-1];
    ts <- xts(data,order.by=t);
    
    return(ts);
}
#==============================

    if (!file.exists(fn)){
        cat("Error: file does not exist\n\t",c(fn), "\n");
        return(0);
    }

    cat("\t Reading file \n\t\t ",as.character(fn) ,"\n");
    if (pihmver > 2.3){
        ts <- readpihmmf(fn,binary);
    }
    else{
        ts <- readpihm20(fn);
    }

}


