#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#'  <- ============================================
#' @param  inpath 
#' @param projectname
#' @keywords PIHM, output
#' @export  all data;
#' @return out A list of all output in outpath. out$names out$ET1 etc.
#' @examples
#' loadoutputQ(outpath,projectname)

plotall <-function(data, if.save=FALSE){
    if (missing(data)){
        pihm <- loadoutput();
    }
    else{
        pihm <- data;
    }
    if (!file.exists(Resultpath)){  #make the result folder
        dir.create(Resultpath)
    }
    
    nm=names(pihm);
    for ( i in 1:length(pihm) ){
        if (!is.null(pihm[[i]])){
            x11()
            cat(i,"\tplotting ",nm[i],"\n");
            if (if.save) {
                imgfile=file.path(Resultpath,paste(as.character(nm[i]),".png",sep=''));
                png(imgfile,width=1000, height=1000)
            }
            matplot(pihm[[i]],type='l',ylab=nm[i])
            if (if.save) {
                dev.off()
            }
        }
    }
}

pihmplot <- function (ts ){
    

}

