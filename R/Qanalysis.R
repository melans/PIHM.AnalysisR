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

anaQ<-function(rivid,siteid=getsiteid(), obsQ,ifP=FALSE,spinupyears=1,calibyears) {
    para<-readpara(TRUE);    
    start<-para[[which(grepl('^start',tolower(names(para))))]];
    end<-para[[which(grepl('^end',tolower(names(para))))]];
    dt <- para[[which(grepl('^rivflx1',tolower(names(para))))]];
    riv=readriv(bak=TRUE);
    if (missing(rivid)){
    outlet=riv$River$outlets;
    }else{
        outlet=rivid
    }
#======1 hydrograh ==================    
    q=goQ(outlets=outlet,if.plot=TRUE,if.update=FALSE,ifP=ifP); #m^3/s
    qd=q*dt         #m^3/day
    t=time(q);
    
sut=as.POSIXct(paste(year(start)+spinupyears, '-01-01', sep='') )        #spin up peiod. 
if (missing(calibyears)){
    calibyears=round((year(t[length(t)])-year(sut) )/2 )
    if (calibyears<1){
        calibyears=1
    }
}
    clt=as.POSIXct(paste(year(start)+spinupyears+calibyears, '-01-01', sep='') )

    validyears=(year(t[length(t)])-year(clt) )
    if (validyears<=0){
        validyears=0
    }



  #  q9=goQ(if.plot=TRUE,if.update=FALSE,ifP=TRUE, Q.number=9); #m/s
#======2 verse OBS ==================    
if.obs=FALSE;
    if (missing(obsQ)){
        if(grepl('^unknow',tolower(siteid))){
            if.obs=FALSE
        }else{
            OBS<-readUSGSQ(siteid=siteid,sdate=as.character(start),edate=as.character(end));    
            if.obs=TRUE;
        }
    }else{
        OBS<-obsQ
        if.obs=TRUE;
    }
if(!if.obs){
    return(q)
}

    time(OBS)=as.POSIXct(time(OBS))
    ct=t[t %in% time(OBS)]    #commont time btw obs and q.
    #ct=ct[which(ct>=sut)];

    
    obs<-OBS[ct];
    time(obs)=ct;    
    ct=ct[which(ct>=sut)]
    imgfile="Discharge_obv_mean.png"

    pihm.hydroplot(obs,fn=imgfile,if.save=TRUE, FUN=mean, ylab= "Q", var.unit = "m3/s")
   #======2.2 daily ==================    
     QvsO(q[ct],obs[ct],fn='Disch.vs.Obser_daily.png');
     image.control(fn='Disch.FDC_daily.png')
     xx=cbind(obs[ct],q[ct]);
     colnames(xx)=c('Obs','Sim')
     qfdc=fdc(xx,lQ.thr=0.1,hQ.thr=0.9,thr.shw=TRUE,col=c('blue','red'))
     grid()
     dev.off()
     
     LineFit(x=as.numeric(obs[ct]),y=as.numeric(q[ct]), fn='Disch.vs.Obser_daily_LineFit.png')

 
      #======2.2.1 monthly ==================   
    if (diff(range(year(ct)))>=1) {
        qm<-apply.monthly(q[ct],FUN=mean);
        obsm<-apply.monthly(obs[ct],FUN=mean);

        QvsO(qm,obsm,fn='Disch.vs.Obser_monthly.png');
    }
    LineFit(x=as.numeric(obsm),y=as.numeric(qm),if.save=TRUE,fn='Disch.vs.Obser_monthly_LineFit.png');
         
      #======2.2.2 weekly ==================   
    if (diff(range(year(ct)))>=1) {
       qw<-apply.weekly(q[ct],FUN=mean);
        obsw<-apply.weekly(obs[ct],FUN=mean);

        QvsO(qw,obsw,fn='Disch.vs.Obser_weekly.png');
    }
    LineFit(x=as.numeric(obsw),y=as.numeric(qw),if.save=TRUE,fn='Disch.vs.Obser_weekly_LineFit.png');

#======3 Q vs OBS qqplot ==================    
 #   image.control(fn='Disch.vs.Obser_qqplot.png')
 #   LineFit(x=as.numeric(obs[ct]),y=as.numeric(q[ct]), fn='Disch.vs.Obser_daily_FDC.png')
  #  qqplot(x=as.numeric(obs[ct]),y=as.numeric(q[ct]), col='blue',log='xy')
  #  abline(0,1,col='grey');
  #  grid()
    #dev.off()

#======4 Warmup/ Calibration /Validation ==================    

t0=t[which(t>=start & t<=sut)];
t1=t[which(t>=sut & t<=clt)];
if (length(t1)>365){
    require(gplots);
    t2=t[which(t>=clt & t<=end)];
    tt=cbind(gof(q[t1],obs[t1]),gof(q[t2],obs[t2]) )
    tt=tt[c('RMSE','NSE','R2'),]
    colnames(tt)=c('Calibration','Validation');

    fn=paste('Discharge_WCV.',spinupyears, '_',calibyears,'_',validyears,'.png',sep='')

    image.control(fn=fn, path=Resultpath)
    plot(obs)

    lines(q[t0],col='blue')
    lines(q[t1],col='red')
    lines(q[t2],col='green')
    addtable2plot(ct[1],max(obs)*0.8,tt,display.rownames=TRUE,display.colnames=TRUE,hlines=TRUE)

    dev.off();
    #legend(c('Observation','Simulation(Spinup)','Simulation(Calibration)','Simulation(Vadilation)'),
    #       col=c('black','blue','red','green'))
}
#======5 Discharge Ratio ==================    
    if (ifP){
        prcp=readprcp();
        Pdata <- apply.daily(prcp/1000, FUN=mean) * 86400; #mm to meter.
        ptime=time(Pdata);
        pt=round(ptime,'days');
        Prcp=xts(Pdata,order.by=pt);
        iP=Prcp[ct]  #precipitation   
        
        
        iP=matrix(colSums(Prcp[ct]),nrow=1)  #precipitation   
        dim(iP)
        
        att=readatt(bak=TRUE);
        iarea=readarea(bak=TRUE);
        area=sum(iarea);
        mid <- att[,which(grepl('^meteo',tolower(colnames(att))))];
        P = sum( matRowMulti( t(as.matrix(iP[,mid]) ) , iarea) ) /area;
        
        Psum=sum(P);
        Qsum=sum(q[ct])/sum(iarea) *86400 ;


        Osum=sum(obs[ct])/sum(iarea) *86400 ;
        
        qr=Qsum/Psum * 100;
        or=Osum/Psum * 100;
        message('Q/P=',round(qr,digits=2),'%\t', 'Obs/P=',round(or,digits=2),'%\n');

        ret=c(qr, or);
        names(ret)=c('Q/P','Obs/P')
        return(ret);
    }else{
        return(list('q'=q[ct],'oq'=obs[ct], 'ct'=ct));
    }
}


getsiteid <- function(id=projectname){
    if (grepl('^lc',id) ){
        id='01576754'
    }else{
        id='unknow';
    }
    return(id)
}
QvsO <-function(q,obs,fn,path=Resultpath,holdon=FALSE, ylab='Q'){
    wd=40;
    ht=30;
     
    if (missing(fn)){
        fn='QvsObs.png'
    }
    imgfile=file.path(path,fn)
    if(grepl('png$',tolower(fn)) ){
        png(imgfile,units = 'cm',width=wd, height=ht, res = 100)
    } else if ( grepl('eps$',tolower(fn))){
         eps(imgfile,units = 'cm',width=wd, height=ht, res = 100)
    }
    else{
        png(imgfile,units = 'cm',width=wd, height=ht, res = 100)
    }
    sim=q;
    ggof(sim=sim,obs=obs,col=c( 'blue','red'), ylab=ylab)
    if (!holdon){
        dev.off()
    }
}

SepBaseFlow<-function(x, filter=0.925,pass=1, unit='', if.save=FALSE,name='X'){
    bt=as.matrix( BaseflowSeparation( as.numeric(x) ,filter_parameter=filter, passes=pass), ncol=2) ;
    q=cbind(as.matrix(x), bt)
    t=time(x);
    q=as.xts(q,order.by=t);

    colnames(q)=c('Q','Q_Base','Q_Quick')


    fn=paste('Q_',name,"_", 
             as.character.Date(t[1],format='%Y%m%d'),"_", 
             as.character.Date(t[length(t)],format='%Y%m%d'),"_",
             'F',filter, 'P',pass
             ,'.png', sep='');

    if (if.save){
    image.control(fn=fn, path='./')
    }
    plot.zoo(q, screen=1, type='l' , lty = c(1, 3, 5), col = rainbow(3), xlab='Time', ylab=paste('Q', unit))
    legend('right', c("Q", "Q_base", "Q_Quick"),
                lty = c(1, 3, 5), col = rainbow(3))
    mtext(side=3, paste('filter = ',filter, '\npass = ', pass))

    grid()

    if (if.save){
        dev.off();
    }
    return(q);

}


Qratio <- function(q, area, col.names='Q/P', period='all'){
    stop('not yet');
    prcp=readprcp();
    t=time(q);
    Pdata <- apply.daily(prcp /1000, FUN=mean) * 86400; #mm to meter.
    ptime=time(Pdata);
    pt=round(ptime,'days');
    Prcp=xts(Pdata,order.by=pt);
    P=Prcp[t]     #precipitation    

    if (grepl('year',tolower(period))){
        func=pihm.yearly;   
    } else {
        func=pihm.wholeperiod;
    }

    Psum=func(P);

    if (missing(area)){
        iarea=readarea(bak=TRUE);
        area=sum(iarea);
    }
    Qsum=matRowMulti(func(q,FUN=sum),1/area) *86400 ;
    qr=matRowMulti(Qsum,1/Psum) * 100;
    
    ret=qr;
    names(ret)=col.names;
    return(ret);
}
lm_eqn <- function(x,y){
    m <- lm(y ~ x);
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
         list(a = format(coef(m)[1], digits = 2), 
              b = format(coef(m)[2], digits = 2), 
             r2 = format(summary(m)$r.squared, digits = 3)))
    eq[1]=paste('Y=',round(coef(m)[1],2), '+', round(coef(m)[2],2),'X',  sep='')
    eq[2]=paste('R2=',format(summary(m)$r.squared, digits = 3), sep='')
    return(eq)
#    as.character(as.expression(eq));                 
}


ObjFcnFDC <- function(q,oq, n=100){
    qq=cbind(q,oq);
    fv = fdc(qq)
    perc=(0:(n-1))/n;
    
    x=fv[,1]
    d=qq[,1];
    Q=numeric(n);
    for (i in 1:n){
        oid = which(x> perc[i])
        Q[i]=max(d[oid])
    }
    Qs=Q;
    
    x=fv[,2]
    d=qq[,2];
    Q=numeric(n);
    for (i in 1:n){
        oid = which(x> perc[i])
        Q[i]=max(d[oid])
    }
    Qo=Q;
    
    CE= 1- (Qs-Qo)/(Qo)
    

}
