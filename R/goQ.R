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


goQ <-function(outlets,if.plot=TRUE,if.update=TRUE,ifP=FALSE,
               Q.number=1,if.river=TRUE){
        ext=paste0('rivFlx',Q.number);
    if (pihmver >=2.5){
        ext=paste0('rivflx',Q.number);
    }
if(if.river){
    if (exists('PIHMOUT')  & !if.update){
        q=PIHMOUT[[ext]];
    }else{
            q=readout(ext=ext,binary=TRUE);
    }
    if ( missing(outlets) ){
        riv <-readriv(bak=TRUE)
        outlets <- riv$River$outlets
    }

    Q <- q[,outlets];
    nq=length(outlets);
    message("Load Q successfully, outlets=",outlets);
}else{
    if (exists('PIHMOUT')  & !if.update){
        q1=PIHMOUT[['FluxSurf0']]
        q2=PIHMOUT[['FluxSurf1']]
        q3=PIHMOUT[['FluxSurf2']]; 
    }else{
        q1=readout(ext='FluxSurf0',binary=TRUE);
        q2=readout(ext='FluxSurf1',binary=TRUE);
        q3=readout(ext='FluxSurf2',binary=TRUE);
    }
    q=abind(q1,q2,q3, along=3)
    outmat=getoutlets(if.river=FALSE)
    nq=nrow(outmat);
    for( i in 1:nrow(outmat)){
        
        tmpQ<- q[,outmat[i,'CellID1'],outmat[i,'EdgeID1']]
                +q[,outmat[i,'CellID2'],outmat[i,'EdgeID2']] ;
        cn =paste(i,'_',outmat[i,'CellID1'],'.',outmat[i,'EdgeID1'],'_',outmat[i,'CellID2'],'.',outmat[i,'EdgeID2'],sep='');
        colname=tmpQ();
        if(i<=1){
            Q=tmpQ;
        }else{
            Q=cbind(Q,tmpQ)
        }
    }
    Q=as.xts(Q,order.by=time(q1))

    message("Load Q successfully, outlets=",outmat);
}
    t=time(Q);
    yr=year(Q);
    period=diff(range(yr));
    fnhead=paste('Discharge',Q.number,sep='');

    if (if.plot){
        if (!file.exists(Resultpath)){  #make the result folder
            dir.create(Resultpath)
        } 
        if(ifP & nq==1){    #if draw PRCP on plot.
#            P= readprcp() * 86400 #forc$PRCP$ts1 * 86400;
#            time(P)=round(time(P),units='days');
#            pdaily=apply.daily(P,FUN=mean);
#            prcp=readprcp();
#
#            P= readprcp() * 86400;
#            time(P)=round(time(P),units='days');
#            ipd=apply.daily(P,FUN=mean);  
#            att=readatt(bak=TRUE);
#            mid <- att[,which(grepl('^meteo',tolower(colnames(att))))];
            calib=readcalib(bak=TRUE);
#            pdaily=rowSums(matRowMulti(ipd[,mid],iarea))/area * calib['PRCP'];
        pdaily=dailyprcp() *  calib['PRCP'];

            iarea=readarea();
            qq=Q/sum(iarea);
     #   mat=data.frame(t,as.numeric(pdaily[t]),as.numeric(Q))
        
            
          #  hydrograph(mat)
            
            pihm.hydrograph(Q,pdaily,fn=paste(fnhead, '_HydroGraph_CMS.png',sep='') ,P.unit='mm/day',S.unit='m^3/s');
            pihm.hydrograph(qq*86400,pdaily,fn=paste(fnhead, '_HydroGraph_H.png',sep='') ,P.unit='mm/day',S.unit='m/day');
        }else{
            plotzoo(Q,fn=paste(fnhead, '.png',sep='') ,ylab='Q',unit='m^3/s');
        }
        if (nq>1 && period >=1){
            for (i in 1:nq){                
                imgfile=paste("Discharge_mean_",i,".png",sep='') 
                #png(imgfile,width=1600, height=1600)
                pihm.hydroplot(Q[,i],fn=imgfile, if.save=TRUE,FUN=mean, ylab= "Q", var.unit = "m3/s")
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

