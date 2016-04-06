
 if.drawmap=FALSE
#     extlist=c('ET0','ET1','ET2',    #   et  
#              'GW','unsat','snow','surf','IS',  #storage
#              'rivStage','rivGW',   #storage in River
#              'FluxSub0','FluxSub1','FluxSub2', #cell flux of GW
#              'FluxSurf0','FluxSurf1','FluxSurf2',  #cell flux of Surf
#              'rivFlx1' #River discharge
#              )
     if (!exists('spinupyears')){
        spinupyears=2
    }
     if (!exists('period')){
         period='yearly' 
     }
    if (!exists('truncateyears')){
        truncateyears=1
    }

    para <- readpara(bak=TRUE);
    Edt <- para[[which(grepl('^et',tolower(names(para))))]];
    Qdt <- para[[which(grepl('^rivflx1',tolower(names(para))))]];
    
    poro<-getporosity(bak=TRUE,if.calib=TRUE);

        PIHMOUT <- loadoutput();
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
    t=t[-length(t)];
    sut=as.POSIXlt(paste(year(t[1])+spinupyears, '-01-01', sep='') )        #spin up peiod. 1 year to 2 year.
    trt=as.POSIXlt(paste(year(t[length(t)])-truncateyears+1, '-01-01', sep='') )        #truancat peiod. .    
    t=t[which(t>=sut & t<trt)];


    Q <- PIHMOUT$rivFlx1[t,outlets];
    if (grepl('^year',tolower(period))  ){
        pihm.period <- pihm.yearly
        ny=length(apply.yearly(t,FUN=sum));
    }else{
        pihm.period <- pihm.monthly
        ny=length(apply.monthly(t,FUN=sum));
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
    
    ncell=length(iarea);
    yrtag=time(apply.yearly(t,FUN=mean));
    
    
    YcET0 <- pihm.period(ET0*Edt,FUN=sum) ;    #m/year
    YcET1 <- pihm.period(ET1*Edt,FUN=sum) ;
    YcET2 <- pihm.period(ET2*Edt,FUN=sum) ;
    Yt <-  time(YcET0);
    

    YcET <- YcET0+YcET1+YcET2;
    #marea <- t(replicate(ny,iarea));
    
    YwET0 <- xts(rowSums( matRowMulti( YcET0[t] , iarea) ) /area,order.by=Yt);
    YwET1 <- xts(rowSums( matRowMulti( YcET1[t] , iarea) ) /area,order.by=Yt);
    YwET2 <- xts(rowSums( matRowMulti( YcET2[t] , iarea) ) /area,order.by=Yt);
    
#    YwET0 <- xts(rowSums( YcET0[t] * marea) /area,order.by=Yt);
#    YwET1 <- xts(rowSums( YcET1[t] * marea) /area,order.by=Yt);
#    YwET2 <- xts(rowSums( YcET2[t] * marea) /area,order.by=Yt);

    colnames(YwET0)='IC m/yr' 
    colnames(YwET1)='Et m/yr' 
    colnames(YwET2)='Evp m/yr' 
   
#    FLUXSURF <- PIHMOUT$FluxSurf1[t];
#    FLUXSUB <- PIHMOUT$FluxSub1[t];
    FLUXSURF<- (PIHMOUT$FluxSurf1[t]+PIHMOUT$FluxSurf2[t]+PIHMOUT$FluxSurf0[t])*86400# m3/s to m3/day
    FLUXSUB<-(PIHMOUT$FluxSub1[t]+PIHMOUT$FluxSub2[t]+PIHMOUT$FluxSub0[t])*86400 # m3/s to m3/day

 
    Ycfsub <- matRowMulti( pihm.period(FLUXSUB,FUN=sum) ,1/iarea);   #volume to height. m^3 to m  ; per year;
    Ycfsurf <- matRowMulti( pihm.period(FLUXSURF,FUN=sum),1/iarea);#volume to height. m^3 to m  ; per year;
#    Ycfsub <- ( pihm.period(FLUXSUB,FUN=sum) )/marea;   #volume to height. m^3 to m  ; per year;
#    Ycfsurf <- ( pihm.period(FLUXSURF,FUN=sum) )/ marea;#volume to height. m^3 to m  ; per year;
    Qsubyr <- xts( rowSums(matRowMulti( Ycfsub, iarea ) )/area,order.by=Yt);
    Qsurfyr <- xts( rowSums(matRowMulti( Ycfsurf, iarea ) )/area,order.by=Yt);
    names(Qsubyr)='Qsub m/yr'
    names(Qsurfyr)='Qsurf m/yr'
    if (spinupyears <=0){
        iD_is <- periodchange(IS,minit['IS',],period=period) ;
        iD_snow <- periodchange(SNOW,minit['SNOW',],period=period) ;
        iD_surf <- periodchange(SURF,minit['OVERLAND',],period=period) ;
        iD_unsat <- matRowMulti(periodchange(UNSAT,minit['UNSAT',],period=period) , poro[2,]);
        iD_gw <- matRowMulti(periodchange(GW,minit['SAT',],period=period) , poro[2,]);
    }else{
        iD_is <- periodchange(IS,period=period) ;
        iD_snow <- periodchange(SNOW,period=period) ;
        iD_surf <- periodchange(SURF,period=period) ;
        iD_unsat <- matRowMulti(periodchange(UNSAT,period=period) , poro[2,]);
        iD_gw <- matRowMulti(periodchange(GW,period=period) , poro[2,]);
    }
    ds <- iD_gw +iD_unsat +iD_snow +iD_surf +iD_is;
#====prcp===========
    mid <- att[,which(grepl('^meteo',tolower(colnames(att))))];

    prcp=readprcp();
    Pdata <- apply.daily(prcp /1000, FUN=mean) * 86400; #mm to meter.
    ptime=time(Pdata);
    pt=round(ptime,'days');
    Prcp=xts(Pdata,order.by=pt);
    P=Prcp[t] * calib$value['PRCP'];    #precipitation    

    iPyr <- pihm.period(P,FUN=sum)

    YcP <- iPyr[,mid] ;

    cpeq <- YcP- YcET - Ycfsub -Ycfsurf;
#======Delta S, (m)    
    dsH <- xts(rowSums(matRowMulti(ds,iarea)/area), order.by=time(ds)); # Storage change of each cell. (m) of each period,e.g yearly,monthly.
    colnames(dsH)=paste('Delta Strg (m)') 
    
    dcpeq <- xts(rowSums(matRowMulti(cpeq,iarea)/area), order.by=time(ds));  #P-E-Qsurf-QGW of each cell. (m) of each period, e.g yearly, monthly.
    colnames(dcpeq)='P-EQ (m)' 
    if (if.drawmap){
         data=colSums(ds);
         PIHM.triplot(data=data,
                      fn=paste('WB_Storage_',para$START,'_to_',t[length(t)],period,'.png',sep=''),
                      name='Storage Change(m)',
                      title=paste('Delta S',para$START,'to',t[length(t)] )
                      );
         data=colSums(cpeq);
         PIHM.triplot(data=data,
                      fn=paste('WB_PEQ_',para$START,'_to_',t[length(t)],period,'.png',sep=''),
                      name='P-ET-Q(m)',
                      title=paste('P-ET-Q',para$START,'to',t[length(t)] ) 
                      );
    }
    Pyr=rowSums( matRowMulti(YcP, iarea)  )/area
    #==============based on each cells ======================
    #Pyr-Qsubyr-Qsurfyr-YwET0-YwET1-YwET2
    #water balance based on cells. No river in account.
    Y_C_PEQ=cbind(Pyr,Qsubyr,Qsurfyr,YwET0,YwET1,YwET2, (Pyr-Qsubyr-Qsurfyr-YwET0-YwET1-YwET2 ),dsH);
    colnames(Y_C_PEQ)[7]='P-EQ'

    pcPEQ= Y_C_PEQ;
    pcPEQ=100*Y_C_PEQ/(matrix(rep(Y_C_PEQ[,1],ncol(Y_C_PEQ) ),ncol=ncol(Y_C_PEQ))) # Percentage value.


    #=============Whole watershed ; cells+rivers ==============
    rarea=riv$surfArea;
    rivStage=PIHMOUT$rivstage[t];
    rivGW=PIHMOUT$rivGW[t]; 
    if (spinupyears <=0){
        iD_stage <- periodchange(rivStage,rinit['RIVERSTATE',],period=period) ;
        iD_rivGW <- periodchange(rivGW,rinit['SATUNDRIV',],period=period) ;
    }else{
        iD_stage <- periodchange(rivStage,rinit['RIVERSTATE',],period=period) ;
        iD_rivGW <- periodchange(rivGW,rinit['SATUNDRIV',],period=period) ;
    }
    YdsRivV=  storageRiver(H=iD_stage)+storageRiver(H=iD_rivGW);    # in Volume (m3)
    
    Ywds <- xts(rowSums(matRowMulti(ds,iarea )/area)+ rowSums((YdsRivV )/area), order.by=time(ds));   # storage change for WHOLE WATERSHED of period, eg. yearly, monthly.. 
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
    fn=paste('WaterBalance_Bar_Watershed_',period,'.png',sep='');
    data<- pwPEQ[,2:5]
    data[is.infinite(data)]=NaN
    data[is.na(data)]=0;
    #Qyr,YwET0,YwET1,YwET2
    col= colors()[c(28,68,50,25)]
    pihm.barplot(fn=fn,data=data,ylab='Percentage to Precipitation',title='Water balance on watershed',yline=ylines,col=col);
    
    fn=paste('WaterBalance_Bar_Cells_',period,'.png',sep='');
    data<- pcPEQ[,2:6]
    data[is.infinite(data)]=NaN
    data[is.na(data)]=0;
    #Qsubyr,Qsurfyr,YwET0,YwET1,YwET2,
    col=colors()[c(30,128,68,50,25)]
    pihm.barplot(fn=fn,data=data,ylab='Percentage to Precipitation',title='Water banlance on cells',yline=ylines,col=col);

    

