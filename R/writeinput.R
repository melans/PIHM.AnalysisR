#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' @param  inpath Path of output folder.
#' @param  prpjectname.
#' @keywords read input. att file.
#' @return A list. att$att, att$points, att$size etc
#' @export  List of att data.
#' @examples
#' readatt(inpath="./",projectname)


writeatt <-function(){
#    nargin <- nargs();
#    if (nargin <1){
#        cat("\nUsage:\n\t readatt <-function(inpath=\"./\",projectname)\n");
#        cat("\n\n");
#        return(0);
#    }
#    
#    if (substring(inpath,nchar(inpath))=="/"){
#    }else{
#        inpath <- paste(inpath,"/",sep='');    
#    }
#    if (nargin <2){ # default: projenctname can be access from projectName.txt;
#        projectname=scan(paste(inpatth,"projectName.txt",sep=''));
#    }
#
#attfile <- paste(inpath, projectname,".att",sep='');
#write.table(att,attfile,row.names=FALSE, sep='\t',quote=FALSE)
#
#return(matatt);
}

writecalib <- function(calib){
    theFile <- file.path(inpath, paste(projectname,".",'calib',sep=''));
    bakFile <- file.path(inpath, paste(projectname,".",'calib.',as.character(Sys.time()),sep=''));
    file.copy(theFile,bakFile)
    file.create(file=theFile);
    for (i in 1:length(calib$offon) ) {

        if (calib$offon[i]){
            str = paste(names(calib$value[i]),calib$value[i],calib$comments[i],sep='\t') ;
        }else{
            str = paste(paste('#',names(calib$value[i]),sep=''),calib$value[i],calib$comments[i], sep='\t')
        }
            write(x=str,file=theFile,append =TRUE)
    }

}
