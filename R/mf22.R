#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#' @param  inpath
#' @param  outpath 
#' @param  resultpath  For saving image file.
#' @param  ifplot  If plot and save image file. 
#' @param  projectname 
#' @param  outlets Outlets id(s).
#' @keywords discharge, hydrograph
#' @return Q, the discharge of outlets, type=TS.
#' @examples  


mf22 <-function(data,name='unknow'){
    outdir <- file.path(outpath, paste(projectname,'v2.2',sep=''));
    if (!file.exists(outdir) ){
        dir.create(outdir)
    }
    if (is.xts(data) ){
        outfile <- file.path(outdir, paste(projectname,'.',name,'.dat',sep=''));
        message('\tWriting ', outfile);
        write.table(x=data[], file=outfile,quote=FALSE, col.names=FALSE);
    }
    if (is.list(data)){
        n=length(data)
        names <- names(data)
        for (i in 1:n){
            d <- data[[i]];
            name <- names[i];
            if (is.xts(d) ){
                outfile <- file.path(outdir, paste(projectname,'.',name,'.dat',sep=''));          
                message('\tWriting ', outfile);
                write.table(x=d[], file=outfile,quote=FALSE, col.names=FALSE);
            }
        }
    }
}

