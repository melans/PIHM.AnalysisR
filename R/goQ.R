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


goQ <-function(outlets,if.plot=TRUE,if.update=TRUE,ifP=FALSE){
    if (exists('PIHMOUT')  & !if.update){
        q=PIHMOUT$rivFlx1
    }else{
        q=readout(ext='rivFlx1',binary=TRUE);
    }
    if ( missing(outlets) ){
        riv <-readriv(bak=TRUE)
        outlets <- riv$River$outlets
    }

    Q <- q[,outlets];
        print("Load Q successfully");
    t=time(Q);
    nq=length(outlets);
    yr=year(Q);
    period=diff(range(yr));
    if (if.plot){
        if (!file.exists(Resultpath)){  #make the result folder
            dir.create(Resultpath)
        } 
        if(ifP){    #if draw PRCP on plot.
            if(!exists('forc')){
                forc<-readforc();
            }
            P=forc$PRCP$ts1 * 86400;
            time(P)=round(time(P),units='days');
            pdaily=apply.daily(P,FUN=mean);
            
            iarea=readarea();
            qq=Q/sum(iarea);
     #   mat=data.frame(t,as.numeric(pdaily[t]),as.numeric(Q))
        
            
          #  hydrograph(mat)
            
            pihm.hydrograph(Q,pdaily,fn='Discharge_HydroGraph_CMS.png',P.unit='mm/day',S.unit='m^3/s');
            pihm.hydrograph(qq*86400,pdaily,fn='Discharge_HydroGraph_H.png',P.unit='mm/day',S.unit='m/day');
        }else{
            plotzoo(Q,fn='Discharge.png',ylab='Q',unit='m^3/s');
        }
        if (nq>1 && period >=1){
            for (i in 1:nq){
                
                imgfile=paste("Discharge_mean_",i,".png",sep='') 
                png(imgfile,width=1600, height=1600)
                pihm.hydroplot(Q[,i],fn=imgfile, if.save=TURE,FUN=mean, ylab= "Q", var.unit = "m3/s")
        #        plot(Q,type='l')
            }
        }else if (nq==1 & period >1) {
            imgfile="Discharge_mean.png"
            #png(imgfile,width=1600, height=1600)
            pihm.hydroplot(Q,fn=imgfile,if.save=TRUE, FUN=mean, ylab= "Q", var.unit = "m3/s")
            #        plot(Q,type='l')
            if (exists('forc')){
#                pihm.hydrograph (flow=Q,precip=forc$PRCP$ts1*86400,fn='hydrograph.png',P.units='m/d',S.units='m/d');
            }
        }else{
        }
    }
    return(Q)
}

