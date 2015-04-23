#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#'  <- ============================================
#' @param  Path of output folder.
#' @keywords read output
#' @export  output data.
#' @examples
#' PIHM()


goQ <-function(inpath, outpath){
    nargin <- nargs();
    if (nargin <1){
        cat("\nUsage:\n\t data = readout(path_of_output,extension_of_data)\n");
        cat("\n\n");
        return(0);
    }
    

}

readout <-function(fn){
    nargin <- nargs()
    if (nargin <1 || nargin >1){
        cat("\nUsage:\n\t data = readout(fn)\n");
        cat("Where:\ndata = [T,data]\n");
        cat("\n\n");
        return(0);
    }
    
    t <- as.Date(d[,1]);
    data <- d[,-1];
    mn <- dim(data)
#    m <- mn[1];
#    n <- mn[2];
#    d <- data.frame(t,data,m,n);
    d <- data.frame(t,data);
    return(d);
}
