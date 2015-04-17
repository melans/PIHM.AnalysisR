#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu@gmail.com  lzs157@psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#'  <- ============================================
#' @param  inpath and outpath
#' @keywords discharge, hydrograph
#' @export  discharge and a plot.
#' loadoutputQ()

loadoutput <-function(outpath="./",projectname){
    nargin <- nargs();
    if (nargin <1){
        cat("\nUsage:\n\t PIHMout <- loadoutput(outpath=\"./\",projectname)\n");
        cat("\n\n");
        return(0);
    }

    flist <- list.files(path=outpath,pattern=paste(projectname,".*.txt",sep='') );
    fpath <- list.files(path=outpath,pattern=paste(projectname,".*.txt",sep=''), full.names = TRUE );
    dataname <- substring(flist,nchar(projectname) + 2,nchar(flist) - 4)
    
        source("/Users/leleshu/Dropbox/SuperTools/R/PIHM.AnalysisR/R/readout.R");
    out <-list(); # initialize the return list.
    out <- list("names"=dataname);  #Names of data;
    

    for( i in 1:length(fpath)){
        d <- readout(fpath[i])       
        cmd=paste('out$',dataname[i],"=d",sep='')
        eval(parse(text=cmd))
    }
       return(out)
}

