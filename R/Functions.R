#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;

Eudist<-function(pt1,pt2){
    x1=pt1[,1];
    x2=pt2[,1];
    y1=pt1[,2];
    y2=pt2[,2];
    dist<- sqrt((x1-x2)^2+(y1-y2)^2);
    return(dist);
}
    
