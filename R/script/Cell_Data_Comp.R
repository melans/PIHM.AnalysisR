
#CellDataComparison <-function (ext){
if(!exists('ext')){
    stop('Need extention name')
}
message('Extension name =', ext);
if(!exists('ttl')){
    ttl=paste('Mean Storage of', ext)
}

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

#    pattern='lc.150617*';
    if.obs=FALSE
    oq=0;
  
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
    pngfiles <- paste(ext,'_mean','_',tt,'_', projdir,'.png',sep='');


    sd=soildepth(bak=FALSE)
    col=rgb(0,0,0,alpha=0.7) 
    msh=readmesh();
    #DataList <- list();
    meanDat=matrix(0,nrow=np, ncol=msh$size[1])
    if (grepl('gu',ext)){
        #gw=readRDS('GW.RDS')
        #un=readRDS('unsat.RDS')
        gw.m=readRDS('GW_mean.RDS')
        un.m=readRDS('unsat_mean.RDS')
    }
    for (i in 1:np ) {
        message(i,'/',np,'\n');
            if (grepl('gu',ext)){
#                DataList[[i]] <- gw[[i]]+un[[i]];
                meanDat[i,] = gw.m[i,]+un.m[i,];    
            }else{
                qq <- readout(ext,path=projlist[i]); 
                if(dim(qq)[1] <365){
                    warning('Length of data is less than a year. dim(dat) =',dim(Dat))
                    next()
                }
                qtime=as.Date(strptime(time(qq),"%Y-%m-%d"))      # convert time to day format
                time(qq)=qtime;
                Dat=qq[which(qtime >=sut)]

                #DataList[[i]] <- Dat;
                meanDat[i,] = colMeans(Dat, na.rm=TRUE)    
            }
        #image.control(fn=pngfiles[i], path=outdir,wd=35,ht=50)   
        if (length(which(is.nan(meanDat[i,])))>0){
            next()
        }
        if( grepl('GW', ext)){
            PIHM.triplot(data=sd-meanDat[i,], terrain=FALSE, name='Depth', fn=pngfiles[i],title='Mean Ground Water depth', colorFUN=topo.colors,colorreverse=FALSE)
        }else{
            PIHM.triplot(data=meanDat[i,], terrain=FALSE, name='Storage', fn=pngfiles[i],title=ttl, colorFUN=topo.colors,colorreverse=TRUE)
        }
        #dev.off();        
    }
#saveRDS(DataList,paste(ext,'.RDS',sep=''))
saveRDS(meanDat,paste(ext,'_mean.RDS',sep=''))
         PIHM.3Dclose()

#}
