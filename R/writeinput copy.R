#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;

lcreplace <- function(lc){
lc=sort(unique(as.numeric(c(11 ,21 ,22 ,23 ,24 ,31 ,41 ,42 ,43 ,81 ,82 ,90,95 ) )) );
    lccode=sort(unique(as.numeric(c(11,12,
             21,22,23,24,
             31,
             41,42,43,
             51,52,
             71,72,73,74,
             81,82,
             90,95) ) ) );
    
    att <- readatt();
    oid <- att[,'LC'] ;
    ids <- numeric(length(lc));
   for (i in length(lc)){
       ids[i] = which(lc[i] == lccode);
   }
    for( i in length(lc)) {
        att[oid==i,'LC']=ids[i];
    }
   writeatt(att);
}
