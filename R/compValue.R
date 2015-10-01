#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#' =============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#' =============================================
#' @param  Path of output folder.
#' @keywords read output
#' @export  output data.
#' @examples
#' PIHM()

comValue <- function (folder='./',pattern=paste(projectname,'.*',sep=''),ext,id=0, obs, projdir){

#    pattern='lc.150617*';
    if.obs=FALSE

    oq=0;
    if (missing(obs) & grepl('rivflx1',tolower(ext)) ){
        siteid=getsiteid();
        if ( !grepl('unk',siteid) ){
            para<-readpara('FALSE');    
            start<-para[[which(grepl('^start',tolower(names(para))))]];
            end<-para[[which(grepl('^end',tolower(names(para))))]];

            obs <-readUSGSQ(siteid=siteid,sdate=as.character(start),edate=as.character(end));
            obs <- obs[!is.na(obs)];
        if.obs=TRUE
        }
    }else{
    }
    if (missing(projdir)){     
        projlist <- dir( path = folder, pattern = pattern,full.names=TRUE);
        projdir <- dir( path = folder, pattern = pattern,full.names=FALSE);
    }else{
        for(i in 1:length(projdir)){
            nc=nchar(projdir[i]);
            if (grepl('/$',projdir[i])){                
                projdir[i]=substr(projdir[i],1 ,nc-1)
            }
        }
        projlist <- file.path(folder, projdir);
    }
    
    outdir <- file.path(folder,'ScnComparison/');
#    outdir <- file.path(projlist,outdirname);
    dump<- lapply(outdir,function(x) if(!file.exists(x)) dir.create(path= x,showWarnings=FALSE,mode='0777') );
    pngfile <- paste(ext,'_',projdir,'.png',sep='');
        
    

    col=rgb(0,0,0,alpha=0.7) 

    Vlist <- list();
    np <- length(projlist)
    for (i in 1:np ) {
        VV <- readout(ext,path=projlist[i]);
#        if (grepl('^GW',ext)){
#            mesh <- readmesh();
#            VV=VV[,1:mesh$size[1]];
#        }
        if ( length(id) <= 1){
            if (id==0){
                V <- as.xts(lapply(VV,mean),order.by=time(VV));
            }
            else{
                V <- VV[,id];
            }
        }else{
             #V <- as.xts(lapply(VV[,id],mean),order.by=time(VV));
            V <- VV[,id];
        }    
        Vlist[[i]] <- V;
        imagecontrol(path=outdir,fn=pngfile[i],bg='transparent')    
        tsRainbow <- rainbow(ncol(VV))
        qlim=c(min(min(VV,na.rm=TRUE),min(oq,na.rm=TRUE) ),max(max(VV,na.rm=TRUE),max(oq,na.rm=TRUE)) );
        plot.zoo(y = VV, ylab = ext, main = paste(ext), ylim=qlim,
        col = tsRainbow , screens = 1)
        if (if.obs){
            oq<-obs[time(V)];
            time(oq)=time(V);   
            lines(oq,lty=3,col=col,lwd=2);
        }
        grid()
        # Set a legend in the upper left hand corner to match color to return series
        #legend(x = "topright", legend = yname, lty = 1,col = tsRainbow)
        dev.off();

    }
    yname <- character(length(projdir));
    for(i in 1:length(projdir) ){
        yname[i] <- substr(projdir[i],6,nchar(projdir[i]) );
    }
    names(Vlist) <- yname;


    Vsets <- as.zoo(do.call(cbind,Vlist));
    tsRainbow <- rainbow(ncol(Vsets))
  
# Plot the overlayed series
dir.create(outdir,showWarnings=FALSE,mode='0777');
pngfile <- paste(ext,'_','vs','.png',sep='')

imagecontrol(path=outdir,fn=pngfile,bg='transparent')    
if (if.obs){
    qlim=c(min(min(Vsets,na.rm=TRUE ),min(obs)),max(max(Vsets,na.rm=TRUE),max(obs)) )
}else{
    qlim=range(Vsets,na.rm=TRUE)
}
plot(x = Vsets, ylab = ext, main = paste(ext,"comparison #",as.character(np)), ylim=qlim,
        col = tsRainbow, screens = 1)

if (if.obs){
    lines(obs,col=col,lwd=2,lty=3);
}
# Set a legend in the upper left hand corner to match color to return series
legend(x = "topright", legend = yname, lty = 1,col = tsRainbow)
 grid()

dev.off();
  return(Vsets);
} 



comQ <- function (folder='./',pattern=paste(projectname,'.*',sep=''),obs,projdir,reload=FALSE,id,skipyear=0){

#    pattern='lc.150617*';
    if.obs=FALSE
    ext='rivFlx1'
    oq=0;
    if (missing(obs) & grepl('rivflx1',tolower(ext)) ){
        siteid=getsiteid();
        if ( !grepl('unk',siteid) ){
            para<-readpara('FALSE');    
            start<-para[[which(grepl('^start',tolower(names(para))))]];
            end<-para[[which(grepl('^end',tolower(names(para))))]];

            obs <-readUSGSQ(siteid=siteid,sdate=as.character(start),edate=as.character(end));
            obs <- obs[!is.na(obs)];
        if.obs=TRUE
        }
    }else{
    }
    if (missing(projdir)){     
        projlist <- dir( path = folder, pattern = pattern,full.names=TRUE);
        projdir <- dir( path = folder, pattern = pattern,full.names=FALSE);
    }else{
        for(i in 1:length(projdir)){
            nc=nchar(projdir[i]);
            if (grepl('/$',projdir[i])){                
                projdir[i]=substr(projdir[i],1 ,nc-1)
            }
        }
        projlist <- file.path(folder, projdir);
    }
    if(missing(id)){
        riv=readriv();
        id=riv$River$outlets
    }
    outdir <- file.path(folder,'ScnComparison/');
    rdsfile=file.path(outdir,paste(projectname,"_Q_.RData",sep=""));
#    outdir <- file.path(projlist,outdirname);
    dump<- lapply(outdir,function(x) if(!file.exists(x)) dir.create(path= x,showWarnings=FALSE,mode='0777') );
    pngfiles <- paste('Q','_',projdir,'.png',sep='');

    col=rgb(0,0,0,alpha=0.7) 

    Qlist <- list();
    np <- length(projlist)
    gfactors=matrix(1,20,np)
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
        if (!reload){
            Q=Qsets[,i]
        }else{
            qq <- readout('rivFlx1',path=projlist[i]);
            Q<- qq[,id];
            Qlist[[i]] <- Q;
        }
        time(Q)=as.Date(time(Q));
        tq=time(Q);
        to=time(obs);
        ct=tq[tq %in% to]
        if (if.obs & length(ct)>365){
            oq<-obs[ct];
            
#            years=unique(year(time(oq)));
#            days=matrix(365, length(years),1);
#            days[which(leap_year(years))]=366;
#            if(skipyear<1){
#                dstart=round(skipyear*365)
#            }else{
#                dstart=sum(days[1:floor(skipyear)])+round( (skipyear-floor(skipyear))*365);
#            }
#            dend=length(oq);
#            nskip=round(365*skipyear)
            
            oq=oq[ct];
            time(oq)=ct;
            Q=Q[ct];
            time(Q)=ct;
            QvsO(q=Q,obs=oq,fn=pngfiles[i],path=outdir);
            gfactors[,i]=gof(sim=Q, obs=oq)            
        }
    }
    if(reload){
        yname <- character(length(projdir));
        for(i in 1:length(projdir) ){
            yname[i] <- substr(projdir[i],6,nchar(projdir[i]) );
        }
        names(Qlist) <- yname;


        Qsets <- as.zoo(do.call(cbind,Qlist));
    }
    time(Qsets)=as.Date(time(Qsets))
        tq=time(Qsets);
        to=time(obs);
        ct=tq[tq %in% to]                 
    tsRainbow <- rainbow(ncol(Qsets))
    oq<-obs[ct];
    #time(oq)=time(Qsets);  
 
dir.create(outdir,showWarnings=FALSE,mode='0777');
pngfile <- paste('Q','_','vs','.png',sep='')

imagecontrol(path=outdir,fn=pngfile,bg='transparent')   

if (if.obs){
    qlim=c(min(min(Qsets,na.rm=TRUE ),min(obs)),max(max(Qsets,na.rm=TRUE),max(obs)) )
}else{
    qlim=range(Qsets,na.rm=TRUE)
}

plot.zoo(y = Qsets, ylab = 'Discharge (m3/s)',xlab='time', main = paste('Q',"comparison #",as.character(np)), ylim=qlim, col = tsRainbow, screens = 1)

if (if.obs){
    lines(oq,col=col,lwd=2,lty=3);
}
# Set a legend in the upper left hand corner to match color to return series
legend(x = "topright", legend = yname, lty = 1,col = tsRainbow)
 grid()
dev.off();
rownames(gfactors)=c('ME','MAE','MSE','RMSE','NRMSE',
                'PBIAS','RSR','rSD','NSE','mNSE',
                'rNSE','d','md','rd','cp',
                'r','R2','bR2','KGE','VE')

Qsets=cbind(Qsets,as.zoo(oq))
saveRDS(Qsets,file=rdsfile,compress=TRUE);
ret=list('Qs'=Qsets,'gof'=gfactors,'dirnames'=projdir);
return(ret);
}
