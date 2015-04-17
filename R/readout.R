#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu@gmail.com  lzs157@psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#'  <- ============================================
#' @param  Path of output folder.
#' @keywords read output. Could be used for reading mesh and river format.
#' @export  output data.
#' @examples
#' readout()


readout <-function(fn){
    nargin <- nargs()
    if (nargin <1 || nargin >1){
        cat("\nUsage:\n\t data = readout(fn)\n");
        cat("Where:\ndata = [T,data]\n");
        cat("\n\n");
        return(0);
    }
    if (!file.exists(fn)){
        cat("Error: file does not exist\n\t",c(fn), "\n");
        return(0);
    }
    cat("\t Reading file \n\t\t ",as.character(fn) ,"\n");
    d <- read.table(fn);
    t <- as.Date(d[,1]);
    data <- d[,-1];
    ts <- xts(data,order.by=t);
    
    return(ts);
}
