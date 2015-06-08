BasicAna<-function(siteid,fgw, funsat,quiet=FALSE,period='yearly'){
    pihm.mode = PIHM.mode('analysis');
    if(!exists('PIHMOUT') ){
        PIHMOUT<-loadoutput();
        assign('PIHMOUT', PIHMOUT,envir=.GlobalEnv);
    }

    if(grepl('lc',projectname)){
        siteid='01576754';
    }
    ufactor=86400;    

    sd=soildepth(bak=TRUE);
    pihm.plot(round(sd,digits=6),ylab='Soil depth (m)',xlab='Cell ID',fn='soildepth.png');
    msd <- mean(sd);
    fgw=min(msd*1.02,msd+0.5);
    funsat=msd*0.95;

    if (!missing(siteid)){
        qr=anaQ(siteid=siteid);
        #q=goQ(ifplot=FALSE);
    }else{  
        q=goQ(if.plot=TRUE,,if.update=FALSE,ifP=TRUE);
    }

    t=PIHMOUT$CommonTime;
    nt=length(t);
    tlim=range(t);
    message('Simulation between ', tlim[1],' and ', tlim[2],'.\n')
    te=tlim[2];

#    GW=readout('GW');
    GW=PIHMOUT$GW;
    gid=datafilter(GW,filter=fgw, name='GW',ylab='Ground Water',unit='m');
    gw=GW[te,];
    PIHM.triplot(data=gw,cellid=gid,fn=paste('GW_',te,'.png',sep=''),name='Ground water (m)',title=paste('GW at',te) );


#    unsat=readout('unsat');
    unsat=PIHMOUT$unsat
    uid=datafilter(unsat,filter=funsat, name='unsat',ylab='unsat',unit='m');
    PIHM.triplot(data=unsat[te,],cellid=uid,fn=paste('unsat_',te,'.png',sep=''),name='Unsat Zone (m)',title=paste('unsat at',te) );



#    surf <- readout('surf');
    surf=PIHMOUT$surf
    sid=datafilter(surf,filter=0.5, name='surf',ylab='Surf',unit='m');
    PIHM.triplot(data=surf[te,],cellid=sid,fn=paste('surf_',te,'.png',sep=''),name='Surface water (m)',title=paste('Surf at',te) );


#    stage <- readout('rivstage')
    stage =PIHMOUT$rivstage
    stid=datafilter(stage,filter=1, name='Rivstage',ylab='RivStage',unit='m',is.riv=TRUE);

#    infil <- readout('infil');
    infil=PIHMOUT$infil
    plotzoo(infil*ufactor, fn='infil.png',ylab='infil',unit='m/day')

#    Rech <- readout('Rech');
    Rech = PIHMOUT$Rech
    plotzoo(Rech*ufactor, fn='Rech.png',ylab='Rech',unit='m/day')





#    et0<-readout('ET0')*ufactor*1000 ##mm/day
#    et1<-readout('ET1')*ufactor*1000
#    et2<-readout('ET2')*ufactor*1000
    
    et0=PIHMOUT$ET0;
    et1=PIHMOUT$ET1;
    et2=PIHMOUT$ET2;

    plotzoo(et0,fn='ET0.png',ylab='ET0',unit='mm/day');
    plotzoo(et1,fn='ET1.png',ylab='ET1',unit='mm/day');
    plotzoo(et2,fn='ET2.png',ylab='ET2',unit='mm/day');
    met0=xts(rowMeans(et0),order.by=time(et0));
    met1=xts(rowMeans(et1),order.by=time(et1));
    met2=xts(rowMeans(et2),order.by=time(et2));

    pihm.hydroplot(met0,FUN=mean,fn='ET0_Mean.png', ylab='IS',var.unit='mm/day');
    pihm.hydroplot(met1,FUN=mean,fn='ET1_Mean.png', ylab='ET',var.unit='mm/day');
    pihm.hydroplot(met2,FUN=mean,fn='ET2_Mean.png', ylab='Evaporation',var.unit='mm/day');

    Ks=suppressWarnings(Kcalib(calib.bak=TRUE ) )

    if (quiet){
        a=rgl.dev.list()
        for (i in 1:length(a)){
            rgl.close()
        }
    }
    message('Starting callculating water balance');
    wb <- suppressWarnings(wb(if.drawmap=T,period=period) );

    ids=list('gid'=gid,'uid'=uid,'stid'=stid,'sid'=sid);
    
    #========summary===================
    ret <- list('wb'=wb, 'ids'=ids)

    return(ret)

}


