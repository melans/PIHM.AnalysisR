#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#' =============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#' =============================================
#' @param  Path of output folder.
#' @keywords read output
#' @export  output data.
#' @examples
#' PIHM()
 
comQ <- function (folder,pattern ){

    folder ='./output/';
    pattern='example.1505*';
    riv <- readriv();
    id <- riv$River$outlets;
    projlist <- dir( path = folder, pattern = pattern,full.names=TRUE);
    Qlist <- list();
    np <- length(projlist)
    for (i in 1:np ) {
        outpath <- projlist[i];
        qq <- readout('rivFlx1');
        Q<- qq[,id];
        Qlist[[i]] <- Q;

    }
    
    qmat <- t(matrix(unlist(Qlist),nrow=np));
    

}
