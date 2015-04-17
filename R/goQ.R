#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu@gmail.com  lzs157@psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#'  <- ============================================
#' @param  inpath and outpath
#' @keywords discharge, hydrograph
#' @export  discharge and a plot.
#' goQ()

goQ <-function(inpath="./", outpath="./", resultpath="./AnalysisResults/",ifplot=1, projectname=0,outlets=0){
    nargin <- nargs();
    if (nargin <1){
        cat("\nUsage:\n\t data = readout(path_of_output,extension_of_data)\n");
        cat("\n\n");
        return(0);
    }
        source("/Users/leleshu/Dropbox/SuperTools/R/PIHM.AnalysisR/R/readout.R");
    if (projectname==0){ # default: projenctname can be access from projectName.txt;
        projfile=paste(inpath,"projectName.txt",sep='')
        projectname=scan(projfile,what=character(),nlines=1);
    }
    if (outlets==0){
        source("/Users/leleshu/Dropbox/SuperTools/R/PIHM.AnalysisR/R/readriv.R");
        riv <-readriv(inpath,projectname)

        outlets <- riv$River$outlets
    }
    fn=paste(outpath,projectname,".rivFlx1.txt",sep="");

    Qall <- readout(fn);
#    Q <- list("time"=Qall$time, "data"=Qall$data[,outlets],"ids"=outlets);
    Q <- Qall[,outlets];
        print("here");

    if (ifplot){
        if (!file.exists(resultpath)){  #make the result folder
            dir.create(resultpath)
        }
        imgfile=paste(resultpath,"Discharge.png",sep='')
        png(imgfile,width=4, height=4, units="in", res=300)
        plot(Q,type='l')
        dev.off()
    }
    return(Q)
}

