folder='./'
if (!exists('pattern')){
    pattern=paste(projectname,'.*',sep='')
}
reload=FALSE
scnfolder='ScnComparison/'
para<-readpara(bak=FALSE);    
start<-para[[which(grepl('^start',tolower(names(para))))]];
end<-para[[which(grepl('^end',tolower(names(para))))]];
if (!exists('skipyear')){
    skipyear=1
}

sut=as.Date(paste(year(start)+skipyear, '-01-01', sep='') )        #spin up peiod. 1 year to 2 year.
gofnames=c('ME','MAE','MSE','RMSE','NRMSE',
                    'PBIAS','RSR','rSD','NSE','mNSE',
                    'rNSE','d','md','rd','cp',
                    'r','R2','bR2','KGE','VE','PBfdc')
ngof=length(gofnames)
#    pattern='lc.150617*';
    if.obs=FALSE
    ext='rivFlx1'
    oq=0;
        siteid=getsiteid();
        if ( !grepl('unk',siteid) ){
            para<-readpara('FALSE');    
            start<-para[[which(grepl('^start',tolower(names(para))))]];
            end<-para[[which(grepl('^end',tolower(names(para))))]];

            obs <-readUSGSQ(siteid=siteid,sdate=as.character(start),edate=as.character(end));
            obs <- obs[!is.na(obs)];
            if.obs=TRUE
        }
  
        projlist <- dir( path = folder, pattern = pattern,full.names=TRUE);
        projdir <- dir( path = folder, pattern = pattern,full.names=FALSE);

        riv=readriv();
        id=riv$River$outlets
    outdir <- file.path(folder,scnfolder);
    rdsfile=file.path(outdir,paste(projectname,"_Q_.RData",sep=""));
#    outdir <- file.path(projlist,outdirname);
    dump<- lapply(outdir,function(x) if(!file.exists(x)) dir.create(path= x,showWarnings=FALSE,mode='0777') );
    np <- length(projlist)
    tt= formatC(1:np, width = floor(log10(np))+1, format = "d", flag = "0")
    pngfiles <- paste('Q','_',tt,'_', projdir,'.png',sep='');
    fdcfiles <- paste('FDC','_',tt,'_', projdir,'.png',sep='');
    qffiles <- paste('QFDC','_',tt,'_', projdir,'.png',sep='');



    col=rgb(0,0,0,alpha=0.7) 

    Qlist <- list();
    gfactors=matrix(1,ngof,np)
    rownames(gfactors)=gofnames
    if (file.exists(rdsfile) & !reload ){
        Qsets= readRDS(file=rdsfile)
        if (ncol(Qsets)==np){   #ncol matches
            reload=FALSE
        }else{
            reload=TRUE
        }
    }else{
        reload=TRUE #no RDS file or reload required.
    }

    for (i in 1:np ) {
        message(i,'/',np,'\n');
        calib=as.matrix(readcalib(bak=TRUE,folder=projlist[i])$value)
        if (i>1){
            mcalib=cbind(mcalib,calib)
        }else{
            mcalib=calib;
        }
    
        if (!reload){
            Q=Qsets[,i]
        }else{
            qq <- readout('rivFlx1',path=projlist[i]);
            Q<- qq[,id];
            Qlist[[i]] <- Q;
        }
        time(Q)=as.Date(time(Q));
        tq=time(Q);
        len=tq;
        if (if.obs){
            to=time(obs);
            ct=tq[tq %in% to]

            ct=ct[which(ct>=sut)]   #excludes spinup period.
            len=ct;
        }
        message(tq[1], ' to ', tq[length(tq)]);
        if (if.obs & length(len)>36){
            oq<-obs[ct];
            
            oq=oq[ct];
            time(oq)=ct;
            Q=Q[ct];
            time(Q)=ct;
            qvs=cbind(oq,Q);
            imagecontrol(fn=qffiles[i], path=outdir,wd=35,ht=50)
             par(mfrow=c(2,1))
           plot.zoo(qvs,col=c('red','blue'),screen=1); 
#            ggof(sim=Q,obs=oq,col=c('red', 'blue'))
            
           # QvsO(q=Q,obs=oq,fn=pngfiles[i],path=outdir,holdon=TRUE);
            leglines = paste(rownames(calib),'=',calib)
            legend('topleft',leglines,bg="transparent")
          #  dev.off();

            gfactors[1:20,i]=gof(sim=Q, obs=oq)   
            gfactors[21,i]=pihm.pbfdc(sim=Q, obs=oq)  
            #gfactors[21,i]=pbiasfdc(sim=Q, obs=oq,lQ.thr=0.9 ,  hQ.thr=0.05)  
            leglines = paste(rownames(gfactors), '=', round(gfactors[,i],3))
            legend("topright",  legend=leglines )#,inset=c(-0.2,0),)
                   
            colnames(qvs)=c('Observation',paste('Q',i));
          #  imagecontrol(fn=fdcfiles[i], path=outdir);
            qfdc=fdc(qvs,col=c('red','blue'))
           
            
            dev.off();        
            
        }
    }
    if(reload){
        yname <- character(length(projdir));
        for(i in 1:length(projdir) ){
            yname[i] <- substr(projdir[i],1,6)  #,nchar(projdir[i]) );
        }
        names(Qlist) <- yname;
        Qsets <- as.zoo(do.call(cbind,Qlist));
    }
    time(Qsets)=as.Date(time(Qsets))
    tsRainbow <- rainbow(ncol(Qsets))
    saveRDS(Qsets,'Qsets.RDS')
if(if.obs){    
        tq=time(Qsets);
        to=time(obs);
        ct=tq[tq %in% to]                 
        oq<-obs[ct];
    #time(oq)=time(Qsets);  
}
dir.create(outdir,showWarnings=FALSE,mode='0777');
ptstr=gsub('\\*|\\.|=','',pattern)
pngfile <- paste('Q','_','vs',ptstr,'.png',sep='')

imagecontrol(path=outdir,fn=pngfile,bg='transparent')   

if (if.obs){
    qlim=c(min(min(Qsets,na.rm=TRUE ),min(obs)),max(max(Qsets,na.rm=TRUE),max(obs)) )
}else{
    qlim=range(Qsets,na.rm=TRUE)
}

plot.zoo(x = Qsets, ylab = 'Discharge (m3/s)',xlab='time', main = paste('Q',"comparison #",as.character(np), ptstr), ylim=qlim, col = tsRainbow, screens = 1)

if (if.obs){
    lines(oq,col=col,lwd=2,lty=3);
    # Set a legend in the upper left hand corner to match color to return series
    legend(x = "topright", legend = yname, lty = 1,col = tsRainbow)
     grid()
    dev.off();
    rownames(gfactors)=gofnames
    gfactors['NSE',which(gfactors['NSE',]==1)]=NA;

    #=========================== nse==========================
    factors=c('NSE', 'R2', 'PBfdc')

    func=c(max, max, min);

    std=stats(t(mcalib))[3,];
    sid=which(std>0);
    vnames=names(std)[sid];

    ana=gfactors[factors,]
saveRDS(ana,'ana.RDS')
    idlist=list();
tmpgof=gfactors;
tmpgof['PBfdc',]=abs(tmpgof['PBfdc',]); #absolute value of pbias fdc.
    for ( ifa in 1:length(factors) ){
        fact=factors[ifa];
        thefactor=tmpgof[fact,]
        #thefactor[which(nse==1)]=-1
        #thefactor[which(nse<0)]=0;
        bestf=func[[ifa]](thefactor,na.rm=TRUE)
        message('\n\tBest ',fact, ' = ',bestf);
        bestid=which(thefactor>=bestf);
        cat(length(bestid), '@',bestid, '\n');
        idlist[[ifa]]=bestid;
        imagecontrol(path=scnfolder,fn=paste(projectname,fact, '_vs_all.png',sep='') );
        #plot(thefactor,type='l',pch=2,lwd=2);grid();
        plot(thefactor);grid();
        dev.off()
        for (i in 1:length(vnames)){
            fn=paste(projectname,fact,'_vs_',vnames[i],'.png',sep='');
            imagecontrol(path=scnfolder, fn=fn);
            plot(mcalib[vnames[i],], thefactor, 
                 ylab=fact, 
                 xlab=paste('Mulitipler for', vnames[i] ));
            dev.off();
        }
    }

    ta=sort(table(unlist(idlist)),decreasing=TRUE)[1:10]
}
saveRDS(mcalib,file='calib.RDS');

#Qsets=cbind(Qsets,as.zoo(oq))
#saveRDS(Qsets,file=rdsfile,compress=TRUE);
#ret=list('Qs'=Qsets,'gof'=gfactors,'dirnames'=projdir);
