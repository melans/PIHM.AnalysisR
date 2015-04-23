#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#'  <- ============================================
#' @param  inpath 
#' @param  outpath 
#' @param projectname
#' @keywords PIHM, output
#' @export  all data;
#' @return out A list of all output in outpath. out$names out$ET1 etc.
#' @examples
#' loadoutputQ(outpath,projectname,inpath)

loadoutput <-function(outpath="./",projectname,inpath){
#   nargin <- nargs();
#    if (nargin <1){
#        cat("\nUsage:\n\t PIHMout <- loadoutput(outpath=\"./\",projectname)\n");
#        cat("\n\n");
#        return(0);
#    }

    flist <- list.files(path=outpath,pattern=paste(projectname,".*.txt",sep='') );
    fpath <- list.files(path=outpath,pattern=paste(projectname,".*.txt",sep=''), full.names = TRUE );
    dataname <- substring(flist,nchar(projectname) + 2,nchar(flist) - 4)
    
        source("/Users/leleshu/Dropbox/SuperTools/R/PIHM.AnalysisR/R/readout.R");
        source("/Users/leleshu/Dropbox/SuperTools/R/PIHM.AnalysisR/R/readmesh.R");
    out <-list(); # initialize the return list.
    #out <- list("names"=dataname);  #Names of data;
    length(out) <- length(dataname);
    names(out) <- dataname;
    msh <- readmesh(inpath,projectname);
    
    for( i in 1:length(fpath)){
        d <- readout(fpath[i])
        n=msh$size[[1]];
        if (tolower(dataname[i])=="gw"){ # GW includes GW of cell and RiverBed.
            gw=d[,c(1:n)];
            bgw=d[,c(n:ncol(d))];
            out$GW=gw;
            out$Bgw=bgw;
        }
        else{
            out[[i]]=d;
        }
        m=nrow(d);
        n=ncol(d);
        cat("Size=[",m,",",n,"]\n");
        #cmd=paste('out$',dataname[i],"=d",sep='')
        #eval(parse(text=cmd))
    }
       return(out)
}

