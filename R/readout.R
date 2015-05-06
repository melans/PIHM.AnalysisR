#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' @param  fn Full path of input file. The file has to be in matrix format and first colomn is in "YYYY-MM-DD hh:mm".
#' @keywords read output. Could be used for reading mesh and river format.
#' @export  output data.
#' @return A TimeSeries data. This list require support of xts packages.
#' @examples
#' readout(ext='rivFlx1',binary=FALSE)


readout <-function(ext,binary=TRUE){


    
    nargin <- nargs()
    if (nargin <1 ){
        cat("\nUsage:\n\t data = readout(extension, binary=FALSE,ncols)\n");
        cat("Where:\ndata = [T,data]\n");
        cat("\tncols only required when binary=TRUE\n");
        cat("\n\n");
        return(0);
    }

#==============================
readpihmmf <- function(fn,binary){
    if (binary){
        maxn=365*24*10000; # maximum = hourly 10000 year * ncells 
        mesh <- readmesh();
        nc=mesh$size[1];    # number of cells in mesh.
        #x=readBin(fn,what=numeric(),n=maxn*mesh$size[1]);
        d <- t(matrix(readBin(fn,what=numeric(),n=maxn*mesh$size[1]),nrow=nc+1));
        t <- as.POSIXct(strptime(sapply(d[,1],t2time),'%Y-%m-%d %H:%M:%S','UTC') );
        ts <- xts(d[,-1],order.by=t);
    }else{
        d <- read.table(fn);
        t <- as.POSIXct(d[,1],'UTC');
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


    #cat("\t Reading file \n\t\t ",as.character(fn) ,"\n");
    if (pihmver > 2.3){
        if (binary){
            fn=list.files(path=outpath,pattern=paste(projectname,'.',ext,'$',sep=''),full.names=TRUE);
        }else{
            fn=list.files(path=outpath,pattern=paste(projectname,'.',ext,'.txt$',sep=''),full.names=TRUE);
        }
        cat("\t Reading file \n\t\t ",as.character(fn) ,"\n");
        if (length(fn)<=0){
            stop('\n\t Error: File ',paste(projectname,'.',ext,'.txt',sep=''),' does not exist.\n\n')
        }
        if (file.exists(fn)){
            ts <- readpihmmf(fn,binary);
        }else{
            stop('\n\t Error: File ',paste(projectname,'.',ext,'.txt',sep=''),' does not exist.\n\n')
#           cat('\n\t Error: File ',paste(projectname,'.',ext,'.txt',sep=''),' does not exist.\n\n');
        }
    }
    else{
        fn=list.files(path=outpath,pattern=paste(projectname,'.',ext,'*',sep=''),full.names=TRUE);
        ts <- readpihm20(fn);
    }

    return(ts);
}


