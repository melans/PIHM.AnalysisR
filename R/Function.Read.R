#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 10:53:00 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;
#' 
#' 
#'  <- ============================================

getFilePath <- function (ext,  path=outpath, bak=FALSE){  

    if (bak){
        theFile <- list.files(path=path,
                              pattern=glob2rx(paste(projectname,".",ext,'.bak',sep='')),full.names=TRUE);
            if (length(theFile)<=0){
                warning('The ',ext,' file in input folder was read, instead of one in output folder.\n');
                theFile <- list.files(path=inpath,
                                      pattern=glob2rx(paste(projectname,".",ext,'$',sep='')),full.names=TRUE);
            }
    }else{
        theFile <- list.files(path=inpath,
                              pattern=glob2rx(paste(projectname,".",ext,sep='')),full.names=TRUE);
    }
        if (length(theFile) <= 0 || !file.exists(theFile)){
            stop ("\n\n\n File \'", theFile , "\' (.", ext, ")  is missing\n\n");
        }

    return (theFile);
}
