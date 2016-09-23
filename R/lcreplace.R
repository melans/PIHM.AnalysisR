#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;

lcreplace <- function(lc){
    att <- readatt();
lc=sort(unique( att[,'LC'] ) );

    oid <- att[,'LC'] ;
    ids <- numeric(length(lc));
    len <- length(lc);

    for( i in 1:len) {
        att[oid==i,'LC']=lc[i];
    }
   writeatt(att);
}
