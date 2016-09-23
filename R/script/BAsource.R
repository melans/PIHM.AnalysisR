                   quiet=FALSE
                   period='yearly'
                   reload=TRUE         
    if (!exists('spinupyears')){
        spinupyears=2
    }
    if (!exists('truncateyears')){
        truncateyears=1
    }
    pihm.mode = PIHM.mode('analysis');
    if(!exists('PIHMOUT') | reload ){
        PIHMOUT<-loadoutput();
        assign('PIHMOUT', PIHMOUT,envir=.GlobalEnv);
    }else{        
    }

    if(grepl('lc',projectname)){
        siteid='01576754';
    }
    ufactor=86400;    
    iarea=readarea();
    
    sd=soildepth(bak=TRUE);
    pihm.plot(round(sd,digits=6),ylab='Soil depth (m)',xlab='Cell ID',fn='soildepth.png');
    msd <- mean(sd);
    fgw=min(msd*1.02,msd+0.5);
    funsat=msd*0.95;

    q=goQ(if.plot=TRUE,,if.update=FALSE,ifP=TRUE);
    aq=anaQ(spinupyears=spinupyears)

    t=PIHMOUT$CommonTime;
    sut=as.POSIXlt(paste(year(t[1])+spinupyears, '-01-01', sep='') )        #spin up peiod. 1 year to 2 year.
    trt=as.POSIXlt(paste(year(t[length(t)])-truncateyears+1, '-01-01', sep='') )        #truancat peiod. .    
    t=t[which(t>=sut & t<trt)];

    
    nt=length(t);
    tlim=range(t);
    message('Simulation between ', tlim[1],' and ', tlim[2],'.\n')
    te=tlim[2];

#    GW=readout('GW');
    GW=PIHMOUT$GW[t];
    gid=datafilter(GW,filter=sd, name='GW',ylab='Ground Water',unit='m');
    gw=GW[te,];
    PIHM.triplot(data=gw,cellid=gid,fn=paste('GW_',te,'.png',sep=''),name='Ground water (m)',title=paste('GW at',te) );

#    unsat=readout('unsat');
    unsat=PIHMOUT$unsat[t]
    uid=datafilter(unsat,filter=sd*0.95, name='unsat',ylab='unsat',unit='m');
    PIHM.triplot(data=unsat[te,],cellid=uid,fn=paste('unsat_',te,'.png',sep=''),name='Unsat Zone (m)',title=paste('unsat at',te) );



#    surf <- readout('surf');
    surf=PIHMOUT$surf[t]
    sid=datafilter(surf,filter=0.5, name='surf',ylab='Surf',unit='m');
    PIHM.triplot(data=surf[te,],cellid=sid,fn=paste('surf_',te,'.png',sep=''),name='Surface water (m)',title=paste('Surf at',te) );

#    stage <- readout('rivstage')
    stage =PIHMOUT$rivstage[t]
    stid=datafilter(stage, name='Rivstage',ylab='RivStage',unit='m',is.riv=TRUE);

#    infil <- readout('infil');
    infil=PIHMOUT$infil[t]*ufactor
    plotzoo(infil, fn='infil.png',ylab='infil',unit='m/day')
#    prcp=readprcp();
#            P= prcp * ufactor;
#            time(P)=round(time(P),units='days');
#            ipd=apply.daily(P,FUN=mean);  
#            att=readatt(bak=TRUE);
#            mid <- att[,which(grepl('^meteo',tolower(colnames(att))))];
#    pdaily=rowSums(matRowMulti(ipd[,mid],iarea))/area;
    calib=readcalib(bak=TRUE);
    pdaily=dailyprcp() *  calib$value['PRCP'];
 
    
            
    minfil=xts(rowMeans(infil), order.by=time(infil));    
    pihm.hydrograph(minfil*1000,pdaily,fn='Infil.vs.Prcp.png',P.unit='mm/day',S.unit='mm/day');


#    Rech <- readout('Rech');
    Rech = PIHMOUT$Rech[t]
    plotzoo(Rech*ufactor, fn='Rech.png',ylab='Rech',unit='m/day')

# Flux Surf
    if('FluxSurf0' %in% names(PIHMOUT)){
    qs=matRowMulti((PIHMOUT$FluxSurf0[t] +PIHMOUT$FluxSurf1[t] +PIHMOUT$FluxSurf2[t]), 1/iarea) *ufactor
    
    plotzoo(qs, fn='SurfaceFlux.png',ylab='Surface Flux',unit='m/day')
    mqs=xts(rowSums(qs), order.by=time(qs));    
    pihm.hydrograph(mqs*1000,pdaily,fn='SurfaceFlux.vs.Prcp.png',P.unit='mm/day',S.unit='mm/day');
    }
# flux sub    

    if('FluxSub0' %in% names(PIHMOUT)){
  qsub=matRowMulti((PIHMOUT$FluxSub0[t] +PIHMOUT$FluxSub1[t] +PIHMOUT$FluxSub2[t]), 1/iarea) *ufactor
    plotzoo(qsub, fn='SubSurfaceFlux.png',ylab='Sub-Surface Flux',unit='m/day')

                   }

#    et0<-readout('ET0')*ufactor*1000 ##mm/day
#    et1<-readout('ET1')*ufactor*1000
#    et2<-readout('ET2')*ufactor*1000
    
    et0=PIHMOUT$ET0[t] * 1000*ufactor;
    et1=PIHMOUT$ET1[t] * 1000*ufactor;
    et2=PIHMOUT$ET2[t] * 1000*ufactor;

    plotzoo(et0,fn='ET0.png',ylab='ET0',unit='mm/day');
    plotzoo(et1,fn='ET1.png',ylab='ET1',unit='mm/day');
    plotzoo(et2,fn='ET2.png',ylab='ET2',unit='mm/day');
    met0=xts(rowMeans(et0),order.by=time(et0));
    met1=xts(rowMeans(et1),order.by=time(et1));
    met2=xts(rowMeans(et2),order.by=time(et2));

    pihm.hydroplot(met0,FUN=mean,fn='ET0_Mean.png', ylab='IS',var.unit='mm/day', if.save=TRUE);
    pihm.hydroplot(met1,FUN=mean,fn='ET1_Mean.png', ylab='ET',var.unit='mm/day', if.save=TRUE);
    pihm.hydroplot(met2,FUN=mean,fn='ET2_Mean.png', ylab='Evaporation',var.unit='mm/day', if.save=TRUE);

    Ks=suppressWarnings(Kcalib(calib.bak=TRUE,quiet=quiet ) )

    if (quiet){
        PIHM.3Dclose();
    }
    message('Starting callculating water balance');
    wb <- suppressWarnings(waterbalance(if.drawmap=T,period=period,skipyears=spinupyears,truncateyears=truncateyears) );
    ids=list('gid'=gid,'uid'=uid,'stid'=stid,'sid'=sid);
     if (quiet){
        PIHM.3Dclose();
    }   
    #========summary===================
    ret <- list('wb'=wb, 'ids'=ids, "Ks"=Ks, 'calib'=readcalib(bak=TRUE))
    image.off()
    
#    return(ret)



