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
        image.control(path=outdir,fn=pngfile[i],bg='transparent')    
        tsColors <- terrain.colors(ncol(VV))
        qlim=c(min(min(VV,na.rm=TRUE),min(oq,na.rm=TRUE) ),max(max(VV,na.rm=TRUE),max(oq,na.rm=TRUE)) );
        plot.zoo(y = VV, ylab = ext, main = paste(ext), ylim=qlim,
        col = tsColors , screens = 1)
        if (if.obs){
            oq<-obs[time(V)];
            time(oq)=time(V);   
            lines(oq,lty=3,col=col,lwd=2);
        }
        grid()
        # Set a legend in the upper left hand corner to match color to return series
        #legend(x = "topright", legend = yname, lty = 1,col = tsColors)
        dev.off();

    }
    yname <- character(length(projdir));
    for(i in 1:length(projdir) ){
        yname[i] <- substr(projdir[i],nchar(projectname)+1,nchar(projdir[i]) );
    }
    names(Vlist) <- yname;


    Vsets <- as.zoo(do.call(cbind,Vlist));
    tsColors <- terrain.colors(ncol(Vsets))
  
# Plot the overlayed series
dir.create(outdir,showWarnings=FALSE,mode='0777');
pngfile <- paste(ext,'_','vs','.png',sep='')

image.control(path=outdir,fn=pngfile,bg='transparent')    
if (if.obs){
    qlim=c(min(min(Vsets,na.rm=TRUE ),min(obs)),max(max(Vsets,na.rm=TRUE),max(obs)) )
}else{
    qlim=range(Vsets,na.rm=TRUE)
}
plot(x = Vsets, ylab = ext, main = paste(ext,"comparison #",as.character(np)), ylim=qlim,
        col = tsColors, screens = 1)

if (if.obs){
    lines(obs,col=col,lwd=2,lty=3);
}
# Set a legend in the upper left hand corner to match color to return series
legend(x = "topright", legend = yname, lty = 1,col = tsColors)
 grid()

dev.off();
  return(Vsets);
} 

comSensitivity <- function(path='./',reload=TRUE, skipyear=2,years=9999){
        trials <- readtrial();
siteid=getsiteid();
      para<-readpara('FALSE');    
            start<-para[[which(grepl('^start',tolower(names(para))))]];
            end<-para[[which(grepl('^end',tolower(names(para))))]];

            obs <-readUSGSQ(siteid=siteid,sdate=as.character(start),edate=as.character(end));
            obs <- obs[!is.na(obs)];

    intrials = unlist(lapply(trials, function(x) length(x)))
    idpara =which(intrials>1);   #id of parameters in sensitivity test
    paranames= names(intrials[idpara])
    npara =length(idpara) #number of parameters in sensitivity test.
    pt=paste('*.',paranames,'=*',sep='');
    for(i in 1:npara){ 
        ret=comQ(folder=path,pattern=pt[i],obs=obs,
                 reload=reload,skipyear=skipyear,years=years)
    }

}

comQ <- function
(folder='./',pattern=paste(projectname,'.*',sep=''),obs,projdir,reload=FALSE,id,skipyear=0,years=99999){
ptstr=gsub('\\*|\\.|=','',pattern)
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
        }else{
            if.obs=FALSE
        }
    }else{
        if.obs=TRUE
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
    gofnames=c('ME','MAE','MSE','RMSE','NRMSE',
                    'PBIAS','RSR','rSD','NSE','mNSE',
                    'rNSE','d','md','rd','cp',
                    'r','R2','bR2','KGE','VE','PBfdc')
    np <- length(projlist)
    if(np<=0){
        return(0)
    }
    if(missing(id)){
        riv=readriv();
        id=riv$River$outlets
    }
    outdir <- file.path(folder,'ScnComparison/');
    
    rdsfile=file.path(outdir,paste(projectname,"_Q",ptstr,"_.RData",sep=""));
#    outdir <- file.path(projlist,outdirname);
    dump<- lapply(outdir,function(x) if(!file.exists(x)) dir.create(path= x,showWarnings=FALSE,mode='0777') );
    pngfiles <- paste('Q','_',projdir,'.png',sep='');

    col=rgb(0,0,0,alpha=0.7) 

    Qlist <- list();
    gfactors=matrix(1,length(gofnames),np)
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
        sy=as.POSIXlt(ct[1]);
        sy$year=sy$year+skipyear;
        ct=ct[which(ct>=as.Date(sy))];
        ey=as.POSIXlt(ct[1]);
        ey$year=ey$year+years;
        ct=ct[which(ct<as.Date(sy))];
        
        if (if.obs & length(ct)>365){
                       oq<-obs[ct];
            
            oq=oq[ct];
            time(oq)=ct;
            Q=Q[ct];
            time(Q)=ct;
            qvs=cbind(oq,Q);
            image.control(fn=pngfiles[i], path=outdir,wd=35,ht=50)
             par(mfrow=c(2,1))
           plot.zoo(qvs,col=c('red','blue'),screen=1); 
#            ggof(sim=Q,obs=oq,col=c('red', 'blue'))
            calib=as.matrix(readcalib(bak=TRUE,folder=projlist[i])$value) 
           # QvsO(q=Q,obs=oq,fn=pngfiles[i],path=outdir,holdon=TRUE);
            leglines = paste(rownames(calib),'=',calib)
            legend('topleft',leglines,bg="transparent")
          #  dev.off();

            gfactors[1:20,i]=gof(sim=Q, obs=oq)   
            gfactors[21,i]=pbiasfdc(sim=Q, obs=oq)  
            leglines = paste(gofnames, '=', round(gfactors[,i],3))
            legend("topright",  legend=leglines )#,inset=c(-0.2,0),)
                   
            colnames(qvs)=c('Observation',paste('Q',i));
          #  image.control(fn=fdcfiles[i], path=outdir);
            qfdc=fdc(qvs,col=c('red','blue'))
             
        }
    }
    if(reload){
        yname <- character(length(projdir));
        for(i in 1:length(projdir) ){
            yname[i] <- substr(projdir[i],6,nchar(projdir[i]) );
        }
        names(Qlist) <- yname


        Qsets <- as.zoo(do.call(cbind,Qlist));
    }
    time(Qsets)=as.Date(time(Qsets))
        tq=time(Qsets);
        to=time(obs);
        ct=tq[tq %in% to]    
        sy=as.POSIXlt(ct[1]);
        sy$year=sy$year+skipyear;
    ct=ct[which(ct>=as.Date(sy))];
    tsColors <- terrain.colors(ncol(Qsets))
    oq<-obs[ct];
    #time(oq)=time(Qsets);  
 Qmat=Qsets[ct];
 
dir.create(outdir,showWarnings=FALSE,mode='0777');

pngfile <- paste('Q','_','vs',ptstr,'.png',sep='')
image.control(path=outdir,fn=pngfile,bg='white', wd=35,ht=25, res=200)    
if (if.obs){
    qlim=c(min(min(Qmat,na.rm=TRUE),min(obs[ct])),max(max(Qmat,na.rm=TRUE),max(obs[ct])) )
}else{
    qlim=range(Qmat,na.rm=TRUE)
}

plot.zoo(x = Qmat, ylab = 'Discharge (m3/s)',xlab='time', main = paste('Q',"comparison #",as.character(np), ptstr), ylim=qlim, col = tsColors, screens = 1)

if (if.obs){
    lines(oq,col=col,lwd=2,lty=3);
}
# Set a legend in the upper left hand corner to match color to return series
legend(x = "topright", legend = yname, lty = 1,col = tsColors,bg='transparent')
 grid()
dev.off();

#===========
pngfile <- paste('QRange','_','vs',ptstr,'.png',sep='')
image.control(path=outdir,fn=pngfile,bg='white', wd=35,ht=25, res=200)   
y.low <- apply(Qmat,1,min,na.rm=TRUE)
y.high <- apply(Qmat,1,max,na.rm=TRUE)
y.mean <- apply(Qmat,1,mean,na.rm=TRUE)
y.lim=range(c(y.low,y.high,obs));

plot(ct,y.mean,type = 'l',ylim = y.lim, col='blue', ylab = 'Discharge (m3/s)', xlab = 'Time')
#lines(ct,y.low, col = 'grey')
#lines(ct, y.high, col = 'grey')
polygon(cbind(ct, rev(ct)), cbind(y.high, rev(y.low)),
col = "grey", border = NA)
if (if.obs){
    lines(ct,obs[ct], col='red')
}
lines(ct,y.mean,col='blue',type='l')
# Set a legend in the upper left hand corner to match color to return series
legend(x = "topright", legend = yname, lty = 1,col = tsColors,bg='transparent')
 grid()
dev.off();


#rownames(gfactors)=gofnames;
#Qsets=cbind(Qsets,as.zoo(oq))
saveRDS(Qsets,file=rdsfile,compress=TRUE);
ret=list('Qs'=Qsets,'gof'=gfactors,'dirnames'=projdir);
return(ret);
}
