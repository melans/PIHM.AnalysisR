 period='yearly'
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
    FLUXSURF<- (PIHMOUT$FluxSurf1[t]+PIHMOUT$FluxSurf2[t]+PIHMOUT$FluxSurf0[t])*86400
    FLUXSUB<-(PIHMOUT$FluxSub1[t]+PIHMOUT$FluxSub2[t]+PIHMOUT$FluxSub0[t])*86400

 
    Ycfsub <- matRowMulti( pihm.period(FLUXSUB,FUN=sum) ,1/iarea);   #volume to height. m^3 to m  ; per year;
    Ycfsurf <- matRowMulti( pihm.period(FLUXSURF,FUN=sum),1/iarea);#volume to height. m^3 to m  ; per year;
#    Ycfsub <- ( pihm.period(FLUXSUB,FUN=sum) )/marea;   #volume to height. m^3 to m  ; per year;
#    Ycfsurf <- ( pihm.period(FLUXSURF,FUN=sum) )/ marea;#volume to height. m^3 to m  ; per year;

    
    wbP =  YcET +Ycfsub +Ycfsurf
    
    pet0 = YcET0/wbP
    pet1 = YcET1/wbP
    pet2 = YcET2/wbP
    pfx = Ycfsurf /wbP
    pgx = Ycfsub /wbP

    mat= rbind( colMeans( pet0), colMeans( pet1), colMeans( pet2), colMeans(pfx) ,colMeans(pgx)) 
    rownames(mat)=c('IC','ET','Ev','Qsurf', 'Qgw')
    
ntype =4;    
pmat = matrix(0, nrow=nrow(mat), ncol=ntype)
    att=readatt(bak=FALSE)
    lc=att[,4]
for (i in 1:ntype){
    if(i==1){       #suburban
        lcs=c(0,21,22)
    }else    if(i==2){       #urban
        lcs=c(23,24)
    } else if (i==3){
        lcs =c(41,42,43)
    } else if (i==4) {
        lcs = c(81,82) 
    }
    print(lcs)
   id=which(lc %in% lcs)
   
   tmp = rowMeans(mat[,id])
   pmat[,i]=tmp      
}
    rownames(pmat)=c('IC','ET','Ev','Qsurf', 'Qgw')
 colnames(pmat)=c('suburban','urban', 'forest', 'agricultural');
barmat=pmat[rev(1:5),c(3,4,1,2)]
fn=paste(Resultpath,'/WATERBANLANCE.txt',sep='')
write.table(barmat, file=fn)
fn=paste('WATERBANLANCE.png',sep='')
imagecontrol(fn=fn)
barplot(barmat, col=rev(c('green', 'darkgreen','orange','blue','darkblue')),
        ylab='Percentage of water balance', legend = rownames(barmat))
dev.off()
        
 
