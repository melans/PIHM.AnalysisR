#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;


writeatt <-function(att,path=inpath){
    
    theFile <- file.path(path, paste(projectname,".",'att',sep=''));
    bakFile <- file.path(path, paste(projectname,".",'att.',as.character(Sys.time()),sep=''));
    file.copy(theFile,bakFile)
    file.create(file=theFile);
    write.table(x=att,file=theFile,col.names=TRUE,row.names=FALSE,quote=FALSE)
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
