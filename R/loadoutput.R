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

loadoutput <-function(){
#   nargin <- nargs();
#    if (nargin <1){
#        cat("\nUsage:\n\t PIHMout <- loadoutput(outpath=\"./\",projectname)\n");
#        cat("\n\n");
#        return(0);
#    }
    if (pihmver >2.3){
        flist <- list.files(path=outpath,pattern=paste(projectname,".*.dat$",sep='') );
        fpath <- list.files(path=outpath,pattern=paste(projectname,".*.dat$",sep=''), full.names = TRUE );
    }else{
        flist <- list.files(path=outpath,pattern=paste(projectname,".*.dat$",sep='') );
        fpath <- list.files(path=outpath,pattern=paste(projectname,".*.dat$",sep=''), full.names = TRUE );

    }
    dataname <- substring(flist,nchar(projectname) + 2,nchar(flist) - 4)
    
    out <-list(); # initialize the return list.
    #out <- list("names"=dataname);  #Names of data;
    length(out) <- length(dataname);
    names(out) <- dataname;
    mesh <- readmesh();
    riv <- readriv(); 
    for( i in 1:length(fpath)){
        if (file.info(fpath[i])$size >0){
            message(fpath[i]);
            nc <- mesh$size[1] ;
            if ( grepl('^riv',tolower(dataname[i])) || grepl('^stage',tolower(dataname[i])) ) {
                nc <- riv$River$size 
            }
            d <- readout( dataname[i],binary=TRUE,nc)
            n=mesh$size[[1]];
            out[[i]]=d;
            m=nrow(d);
            n=ncol(d);
            cat("Size=[",m,",",n,"]\n");
            #cmd=paste('out$',dataname[i],"=d",sep='')
            #eval(parse(text=cmd))
        }else{
            cat('File is empty. ', flist[i],'\n');
        }
    }
       return(out)
}

