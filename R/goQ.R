#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#' @param  inpath
#' @param  outpath 
#' @param  resultpath  For saving image file.
#' @param  ifplot  If plot and save image file. 
#' @param  projectname 
#' @param  outlets Outlets id(s).
#' @keywords discharge, hydrograph
#' @return Q, the discharge of outlets, type=TS.
#' @examples  
#' Q <- goQ(inpath="./", outpath="./", resultpath="./AnalysisResults/",ifplot=1, projectname=0,outlets=0)


goQ <-function(q,outlets,ifplot=FALSE){
    if (missing(q)){
        if (pihmver>2.3){
            q=readout(ext='rivFlx1',binary=TRUE);
        }
    }
    if ( missing(outlets) ){
        riv <-readriv()
        outlets <- riv$River$outlets
    }

#    Q <- list("time"=Qall$time, "data"=Qall$data[,outlets],"ids"=outlets);
    Q <- q[,outlets];
        print("Load successfully");

    if (ifplot){
        if (!file.exists(Resultpath)){  #make the result folder
            dir.create(Resultpath)
        }
        imgfile=file.path(Resultpath,"Discharge.png")
        png(imgfile,width=1600, height=1600)
        plot(Q,type='l')
        dev.off()
    }
    return(Q)
}

