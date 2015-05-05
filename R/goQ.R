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


goQ <-function(q,outlets){
    if (missing(q)){
        if (pihmver>2.3){
            q=readout(ext='rivFlx1');
        }
    }
        riv <-readriv()
        outlets <- riv$River$outlets
    fn=paste(outpath,projectname,".rivFlx1.txt",sep="");
    cat('Reading file: ',fn,'\n');
    Qall <- readout(fn);
#    Q <- list("time"=Qall$time, "data"=Qall$data[,outlets],"ids"=outlets);
    Q <- Qall[,outlets];
        print("Load successfully");

    if (ifplot){
        if (!file.exists(resultpath)){  #make the result folder
            dir.create(resultpath)
        }
        imgfile=paste(resultpath,"Discharge.png",sep='')
        png(imgfile,width=1600, height=1600)
        plot(Q,type='l')
        dev.off()
    }
    return(Q)
}

