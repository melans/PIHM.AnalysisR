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

anaQ<-function(siteid) {
    para<-readpara(TRUE);    
    start<-para[[which(grepl('^start',tolower(names(para))))]];
    end<-para[[which(grepl('^end',tolower(names(para))))]];
    dt <- para[[which(grepl('^rivflx1',tolower(names(para))))]];


#======1 hydrograh ==================    
    q=goQ(if.plot=TRUE,if.update=FALSE,ifP=TRUE); #m/s
    qd=q*dt         #m^3/day
    t=time(q);
#======2 verse OBS ==================    
    OBS<-readUSGSQ(siteid=siteid,sdate=as.character(start),edate=as.character(end));    
    obs<-OBS[time(q)];
    time(obs)=t;    
    imgfile="Discharge_obv_mean.png" 
    pihm.hydroplot(obs,fn=imgfile,if.save=TRUE, FUN=mean, ylab= "Q", var.unit = "m3/s")
    
   #======2.2 daily ==================    
    if (diff(range(year(t)))>=1) {
    QvsO(q,obs,fn='Disch.vs.Obser_dailyy.png');
    
   #======2.2 monthly ==================   
    qm<-apply.monthly(qd,FUN=sum);
    obsm<-apply.monthly(obs*86400,FUN=sum);

    QvsO(qm,obsm,fn='Disch.vs.Obser_monthly.png');
    }
#======3 Discharge Ratio ==================    
     if ( exists('forc') ){
    }else{
        forc=readforc();
    }
    Pdata <- apply.daily(forc$PRCP$ts1 /1000, FUN=mean) * 86400; #mm to meter.
    ptime=time(Pdata);
    pt=round(ptime,'days');
    Prcp=xts(Pdata,order.by=pt);
    P=Prcp[t]     #precipitation    
    Psum=sum(P);
    iarea=readarea(bak=TRUE);
    Qsum=sum(q)/sum(iarea) *86400 ;
    Osum=sum(obs)/sum(iarea) *86400 ;
    
    qr=Qsum/Psum * 100;
    or=Osum/Psum * 100;
    message('Q/P=',round(qr,digits=2),'%\t', 'Obs/P=',round(or,digits=2),'%\n');

    ret=c(qr,or);
    names(ret)=c('Q/P','Obs/P')
    return(ret);
}
siteid <- function(){
    id='unknow';
    if (grepl('^lc',projectname) ){
        id='01576754'
    }
    return(id)
}
QvsO <-function(q,obs,fn){
    wd=40;
    ht=30;
    
    if (missing(fn)){
        fn='QvsObs.png'
    }
    imgfile=file.path(Resultpath,fn)
    if(grepl('png$',tolower(fn)) ){
        png(imgfile,units = 'cm',width=wd, height=ht, res = 100)
    } else if ( grepl('eps$',tolower(fn))){
         eps(imgfile,units = 'cm',width=wd, height=ht, res = 100)
    }
    else{
        png(imgfile,units = 'cm',width=wd, height=ht, res = 100)
    }
    ggof(sim=q,obs=obs)
    dev.off()
 

}

