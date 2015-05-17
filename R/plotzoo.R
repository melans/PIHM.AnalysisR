#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#'

plotzoo <- function(x, screen=1,color,format='png',fn='plotzoo.png'){
    
    if (grepl('^png',tolow(format)) ){
        imgfile=file.path(Resultpath,fn)
        png(imgfile,width=1600, height=1600)
    }
    plot.zoo(x=x,screen=screen);
    dev.off();
    
}
