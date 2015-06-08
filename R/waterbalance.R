#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;

wb <- function(pihmout, period='yearly',if.drawmap=FALSE){
#     extlist=c('ET0','ET1','ET2',    #   et  
#              'GW','unsat','snow','surf','IS',  #storage
#              'rivStage','rivGW',   #storage in River
#              'FluxSub0','FluxSub1','FluxSub2', #cell flux of GW
#              'FluxSurf0','FluxSurf1','FluxSurf2',  #cell flux of Surf
#              'rivFlx1' #River discharge
#              )
    para <- readpara(bak=TRUE);
    Edt <- para[[which(grepl('^et',tolower(names(para))))]];
    Qdt <- para[[which(grepl('^rivflx1',tolower(names(para))))]];
    
    if(!exists('PIHMOUT') & missing(pihmout) ){
        PIHMOUT <- loadoutput();
        assign('PIHMOUT', PIHMOUT,envir=.GlobalEnv);
    }
    att <- readatt(bak=T);
    riv <- readriv(bak=T);
    outlets=riv$River$outlets;
    iarea <- readarea(bak=T);
    area <- sum(iarea);
    init <- readinit(bak=TRUE)
    minit <- t(init$minit);
    rinit <- t(init$rinit);
    rownames(minit)=toupper(rownames(minit));
    rownames(rinit)=toupper(rownames(rinit));
    colnames(minit)=paste('V',1:ncol(minit),sep='');
    calib <- readcalib(bak=TRUE);
    
    t=PIHMOUT$CommonTime;
    Q <- PIHMOUT$rivFlx1[t,outlets];
    if (grepl('^year',tolower(period))  ){
        pihm.period <- pihm.yearly
    }else{
        pihm.period <- pihm.monthly
    }
    if(ncol(Q)>1){
        Q=xts(rowSums(Q),order.by=t);
    }
    
    Qyr= pihm.period(Q*Qdt,FUN=sum) / area;
    if (ncol(Qyr)==1){
        qname=paste('Q','m/yr');
    }else{
        qname=paste('Q',1:ncol(Qyr),'m/yr',sep='');
    }
    colnames(Qyr)= qname;
    
    
    ET0 <- PIHMOUT$ET0[t] ;
    ET1 <- PIHMOUT$ET1[t];
    ET2 <- PIHMOUT$ET2[t];

    GW <- PIHMOUT$GW[t];
    UNSAT <- PIHMOUT$unsat[t];
    SNOW <- PIHMOUT$snow[t];
    SURF <- PIHMOUT$surf[t];
    IS <- PIHMOUT$IS[t]

    RIVSTAGE <- PIHMOUT$rivStage[t];
    RIVGW <- PIHMOUT$rivGW[t];
    

    FLUXSURF <- PIHMOUT$FluxSurf1[t];
    FLUXSUB <- PIHMOUT$FluxSub1[t];
    FLUXSURF <- PIHMOUT$FluxSurf1[t]+PIHMOUT$FluxSurf2[t]+PIHMOUT$FluxSurf0[t]
    FLUXSUB <- PIHMOUT$FluxSub1[t]+PIHMOUT$FluxSub2[t]+PIHMOUT$FluxSub0[t] 
    ny=length(unique(year(t)))
    ncell=length(iarea);
    yrtag=time(apply.yearly(t,FUN=mean));
    
    
    YcET0 <- pihm.period(ET0*Edt,FUN=sum) ;    #m/year
    YcET1 <- pihm.period(ET1*Edt,FUN=sum) ;
    YcET2 <- pihm.period(ET2*Edt,FUN=sum) ;
    Yt <-  time(YcET0);
    

    YcET <- YcET0+YcET1+YcET2;
    ny <- length(unique(year(t)));
    marea <- t(replicate(ny,iarea));

    YwET0 <- xts(rowSums( YcET0[t] * marea) /area,order.by=Yt);
    YwET1 <- xts(rowSums( YcET1[t] * marea) /area,order.by=Yt);
    YwET2 <- xts(rowSums( YcET2[t] * marea) /area,order.by=Yt);

    colnames(YwET0)='IC m/yr' 
    colnames(YwET1)='Et m/yr' 
    colnames(YwET2)='Evp m/yr' 
    
    Ycfsub <- ( pihm.period(FLUXSUB,FUN=sum) )/marea;   #volume to height. m^3 to m  ; per year;
    Ycfsurf <- ( pihm.period(FLUXSURF,FUN=sum) )/ marea;#volume to height. m^3 to m  ; per year;
    Qsubyr <- xts(rowSums(Ycfsub),order.by=Yt);
    Qsurfyr <- xts(rowSums(Ycfsurf),order.by=Yt);
    names(Qsubyr)='Qsub m/yr'
    names(Qsurfyr)='Qsurf m/yr'

    iD_is <- periodchange(IS,minit['IS',],period=period) ;
    iD_snow <- periodchange(SNOW,minit['SNOW',],period=period) ;
    iD_surf <- periodchange(SURF,minit['OVERLAND',],period=period) ;
    iD_unsat <- periodchange(UNSAT,minit['UNSAT',],period=period) ;
    iD_gw <- periodchange(GW,minit['SAT',],period=period) ;

    ds <- iD_gw +iD_unsat +iD_snow +iD_surf +iD_is;
#====prcp===========
    mid <- att[,which(grepl('^meteo',tolower(colnames(att))))];
    if ( exists('forc') ){
    }else{
        forc=readforc();
    }
    Pdata <- apply.daily(forc$PRCP$ts1 /1000, FUN=mean) * 86400; #mm to meter.
    ptime=time(Pdata);
    pt=round(ptime,'days');
    Prcp=xts(Pdata,order.by=pt);
    P=Prcp[t] * calib$value['PRCP'];    #precipitation    
    Pyr <- pihm.period(P,FUN=sum)
    colnames(Pyr)='Prcp m/yr' 

    YcP <- xts(matrix(rep(Pyr,length(mid)),nrow=ny),order.by=Yt);

    cpeq <- YcP- YcET - Ycfsub -Ycfsurf;
#======Delta S, (m)    
    dsH <- xts(rowSums((ds*marea)/area), order.by=time(ds)); # Storage change of each cell. (m) of each period,e.g yearly,monthly.
    colnames(dsH)=paste('Delta Strg (m)') 
    
    dcpeq <- xts(rowSums((cpeq*marea)/area), order.by=time(ds));  #P-E-Qsurf-QGW of each cell. (m) of each period, e.g yearly, monthly.
    colnames(dcpeq)='P-EQ (m)' 
    if (if.drawmap){
         data=colSums(ds);
         PIHM.triplot(data=data,
                      fn=paste('WB_Storage_',para$START,'_to_',t[length(t)],'.png',sep=''),
                      name='Storage Change(m)',
                      title=paste('Delta S',para$START,'to',t[length(t)] )
                      );
         data=colSums(cpeq);
         PIHM.triplot(data=data,
                      fn=paste('WB_PEQ_',para$START,'_to_',t[length(t)],'.png',sep=''),
                      name='P-ET-Q(m)',
                      title=paste('P-ET-Q',para$START,'to',t[length(t)] ) 
                      );
    }

    #==============based on each cells ======================
    #Pyr-Qsubyr-Qsurfyr-YwET0-YwET1-YwET2
    #water balance based on cells. No river in account.
    Y_C_PEQ=cbind(Pyr,Qsubyr,Qsurfyr,YwET0,YwET1,YwET2, (Pyr-Qsubyr-Qsurfyr-YwET0-YwET1-YwET2 ),dsH);
    colnames(Y_C_PEQ)[7]='P-EQ'

    pcPEQ= Y_C_PEQ;
    pcPEQ=100*Y_C_PEQ/(matrix(rep(Y_C_PEQ[,1],ncol(Y_C_PEQ) ),ncol=ncol(Y_C_PEQ))) # Percentage value.


    #=============Whole watershed ; cells+rivers ==============
    rarea=riv$surfArea;
    rivStage=PIHMOUT$rivstage;
    iD_stage <- periodchange(rivStage,rinit['RIVERSTATE',],period=period) ;

    rivGW=PIHMOUT$rivGW;
    iD_rivGW <- periodchange(rivGW,rinit['SATUNDRIV',],period=period) ;
    
    YdsRivV=  storageRiver(H=iD_stage)+storageRiver(H=iD_rivGW);    # in Volume (m3)
    
    Ywds <- xts(rowSums((ds*marea )/area)+ rowSums((YdsRivV )/area), order.by=time(ds));   # storage change for WHOLE WATERSHED of period, eg. yearly, monthly.. 
    colnames(Ywds)='Delta Strg (m)'
    Y_W_PEQ=cbind(Pyr,Qyr,YwET0,YwET1,YwET2,(Pyr-Qyr-YwET0-YwET1-YwET2),Ywds); # P-E-Q for WHOLE WATERSHED of period, eg. yearly, monthly..
    colnames(Y_W_PEQ)[6]='P-EQ'
    

    pwPEQ= Y_W_PEQ;
    pwPEQ=100*Y_W_PEQ/(matrix(rep(Y_W_PEQ[,1],ncol(Y_W_PEQ)),ncol=ncol(Y_W_PEQ)))
    
    
    wblist<-list('cWB'=Y_C_PEQ, 'wWB'=Y_W_PEQ,'PcWB'= pcPEQ, 'PwWB'= pwPEQ )
    
    wbfile <- file.path(Resultpath,paste('WaterBalance',t[1],'to',t[length(t)],'.txt',sep=''));
    write( paste('Water balance between',t[1],'and',t[length(t)],'\n') ,file=wbfile, append=FALSE);    
    write(paste('\nPeriod = ',period, '\n'),file=wbfile,append=TRUE);    
    for(i in 1:length(wblist) ){
        write(paste('\n\n\t',names(wblist)[i]),file=wbfile, append=TRUE);   
        write.table(wblist[[i]],file=wbfile,append=TRUE,row.names=TRUE,col.names=TRUE,quote=FALSE,eol = "\n");
    }

    ylines=rep(100,ny);
    fn='WaterBalance_Bar_Watershed.png';
    data<- pwPEQ[,2:5]
    #Qyr,YwET0,YwET1,YwET2
    col= colors()[c(28,68,50,25)]
    pihm.barplot(fn=fn,data=data,ylab='Percentage to Precipitation',title='Water balance on watershed',yline=ylines,col=col);
    
    fn='WaterBalance_Bar_Cells.png';
    data<- pcPEQ[,2:6]
    #Qsubyr,Qsurfyr,YwET0,YwET1,YwET2,
    col=colors()[c(30,128,68,50,25)]
    pihm.barplot(fn=fn,data=data,ylab='Percentage to Precipitation',title='Water banlance on cells',yline=ylines,col=col);

    
    return(wblist);

}


storageRiver <- function(H){
#convert Height of water in river to Volume. 
    t=time(H);
    nt = length(t);
    riv<-readriv(bak=TRUE);
    calib<-readcalib(bak=TRUE);
    
    sarea<-riv$surfArea * calib$value['RIV_WDTH'];
    
    ma<-t(matrix(rep(sarea,nt),ncol=nt,nrow=riv$River$size ))
    
    vol=xts(H*ma,order.by=t);
    
    return(vol);
    
}
periodchange <- function(x,xinit, period='null'){
#  x=GW[,1:5];    
# xinit=as.zoo(matrix(minit['Sat',1:5],nrow=1))
    if (missing(xinit)){
        xi=x[1,]        #initial value , from first row of x;
    }else{
        xi=as.zoo(matrix(xinit,nrow=1)) #value from init file.
        time(xi)=time(x)[1];    
    }
         nx <- nrow(x);    
    x0 <- x[-nx];    
    time(x0) = time(x0)+ diff(time(x));
    x0=xts(rbind(xi,x0),order.by=time(x));
    dx=x-x0;
    
    if (grepl('^year',tolower(period)) ){
        ds= pihm.yearly(dx,FUN=sum) ;
        ret=ds;
    }else if (grepl('^year',tolower(period)) ){
        ds= pihm.monthly(dx,FUN=sum) ;
        ret=ds;
    }
    else{
        ret=dx;
    }
    
    return(ret);
}

pihm.monthly <- function(x,FUN){
    t=time(apply.monthly(x,FUN=FUN)) ;
#   xx = apply.monthly(x,FUN=FUN);
    xx <- sapply(x,function(x) apply.monthly(x,FUN=FUN)); 
    y <- xts(matrix(xx,nrow=length(t),ncol=ncol(x)),order.by=t);
    
    return(y);
}
pihm.yearly <- function(x,FUN){
    t=time(apply.yearly(x,FUN=FUN)) ;
#   xx = apply.yearly(x,FUN=FUN);
    xx <- sapply(x,function(data) apply.yearly(data,FUN=FUN)); 
    y <- xts(matrix(xx,nrow=length(t),ncol=ncol(x)),order.by=t);
    
    return(y);
}


