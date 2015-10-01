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

writecalib <- function(calib, newfilename){
    if (missing(newfilename)){
        theFile <- file.path(inpath, paste(projectname,".",'calib',sep=''));
        bakFile <- file.path(inpath, paste(projectname,".",'calib.',as.character(Sys.time()),'-',rnorm(1),sep=''));
        file.copy(theFile,bakFile)
    }else{
        theFile <- file.path(inpath, newfilename);
    }
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

writemesh <- function(mesh){
    theFile <- file.path(inpath, paste(projectname,".",'mesh',sep=''));
    bakFile <- file.path(inpath, paste(projectname,".",'mesh.',as.character(Sys.time()),'-',rnorm(1),sep=''));
    file.copy(theFile,bakFile)
    file.create(file=theFile);
    str = c(as.character(mesh$size[1]),colnames(mesh$mesh)[-1]);
    
    write.table(x=str,file=theFile,append=FALSE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");  
    write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x=mesh$mesh,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   
    
    str = c(as.character(mesh$size[2]),colnames(mesh$points)[-1]);
    write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");   
    write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x=mesh$points,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   


}

writeinit <- function(init){
     theFile <- file.path(inpath, paste(projectname,".",'init',sep=''));
    bakFile <- file.path(inpath, paste(projectname,".",'init.',as.character(Sys.time()),'-',rnorm(1),sep=''));
    file.copy(theFile,bakFile)
    file.create(file=theFile);
    write.table(x=init$minit,file=theFile,append=FALSE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\n");  
    write.table(x=init$rinit,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\n");  
}
