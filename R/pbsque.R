#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' @param  
#' @param  
#' @keywords 
#' @return 
#' @export  
#' @examples
pbsaset <-function(pj){
    fd=list.files('../input',pattern=pj,full.names=TRUE);
    if (length(fd)==1){
        inpath=fd;
    }
    outpath='./'
    projectname=pj;
    
    Sys.setenv(TZ = "UTC")
    PIHM(indir=inpath,outdir=outpath,pname=projectname,ver=2.4)
}
pbsset <-function(pj){
    fd=list.files('input',pattern=pj,full.names=TRUE);
    if (length(fd)==1){
        inpath=fd;
    }
    outpath='./'
    projectname=pj;
    
    Sys.setenv(TZ = "UTC")
    PIHM(indir=inpath,outdir=outpath,pname=projectname,ver=2.4)
}
pbs.trial  <- function(pbsfile){
    if(missing(pbsfile)){
        pbsfile=generatepbs();
    }
    trials <- readtrial();
    calib <- readcalib(bak=FALSE);
    sets <- calibSets(trials, calib);
    
    nsets <- length(sets);
    ncores <- 100;
    
    tmp <- character(nsets)
    for(i in 1:nsets){
        tmp[i]<- -1;
    }
   message('\n\n',nsets,' trials are waiting for resources. There are ', ncores,' workers available.\n\n');
    message('Go on ? (yes/no) ');
   line <- readline()
   if ( !grepl('^y', tolower(line)) ){
       stop(' Abort. \n');
   }
   message('\n\nStart parrellel processes\t',Sys.time(),'\n');
   oldque=0;
    for(i in 1:10) #nsets)          
    {   
        lag = round(rnorm(1,ncores*2)*((i-1) %% ncores ) );   #lag time before start PIHM.
#        message('lag = ',lag,'\n');
#        null <- system(paste('sleep', as.character(lag) ) ,wait=TRUE );
        message('Trial #', i, ' starts. \t@',Sys.time(),'\n');
        pbswritecalib(sets[[i]]);
        cmd <- paste('./pihm -d -v ',projectname)
        pbsfile <- writepbs(cmd=cmd);
        #null <- system(paste('qsub',pbsfile) ,ignore.stdout = FALSE, intern=TRUE, ignore.stderr = FALSE);
        null <- system(paste('qsub ',pbsfile) ,ignore.stdout = FALSE, intern=TRUE, ignore.stderr = FALSE);
           
    }
}


pbsPIHM.calibration  <-
    function(keylist=list(),wallhours=24,memory='1gb',
             quiet=TRUE,shnum=1, if.pbs=TRUE,RDSfile){
    #shnum -- number of shell files.
    # if.pbs --- if run the calibration on PBS. default=TRUE. 
    trials <- readtrial();
    if(!quiet){
        ntmp=unlist(lapply(trials,length))
        print(ntmp)
        message('\n\n',prod(ntmp),' trials are waiting for resources. \n')
        message('Go on ? Yes/(No) ');
       line <- readline()
       if ( !grepl('^y', tolower(line)) ){
           stop(' Abort. \n');
       }
    }
    calib <- readcalib(bak=FALSE);
    if (missing(RDSfile)){
        sets <- calibSets(trials, calib,keylist=keylist);
        saveRDS(file='calibMatrix.RDS',sets);
    }else{
        sets <- readRDS(file='calibMatrix.RDS')
    }
    nsets <- length(sets);
    #ncores <- min(nsets,MAX_THREADS);
   
       shfn=paste('pbs',seq(1,shnum,by=1),'.sh',sep='');
   nlines=ceiling(nsets/shnum);
       shlines=character(); 
       shlines[1]='#!/bin/bash';
    ish=1;
    il = 1; # control the line number of shlines.
    
     if(!quiet){
        message('\n\n',nsets,' trials are waiting for resources. \n')
        message( shnum,' shell file(',nlines,' lines each) will be genrated.\n');
        message('Go on ? Yes/(No) ');
       line <- readline()
       if ( !grepl('^y', tolower(line)) ){
           stop(' Abort. \n');
       }
    }
   message('\n\nStart calibration processes\t',Sys.time(),'\n');
   pbslines=character(nsets);
    for(i in 1:nsets)       
    {  
        num=formatC(i, width = ceiling(log10(nsets)), format = "d", flag = "0")
        fn=paste(projectname,'_',num,'.', strftime(Sys.time(),format='%y%m%d', tz='UTC'),sep='');
        calibfn=paste(fn,'.calib', sep='');
        calibpath=file.path(inpath, calibfn)
            Sys.sleep(0.01)

        writecalib(sets[[i]],newfilename=calibfn);
        pbsfn=paste(fn,'.pbs',sep='');
        logfn=paste(fn,'.log',sep='');
    
#        while( file.exists(file=calibpath) | file.exists(file=pbsfn)){V
#            lag = 1 #runif(1,max=5,min=1);   #lag time before start PIHM.
#            Sys.sleep(lag)
#            fn=paste(projectname,'.', strftime(Sys.time(),format='%y%m%d-%H%M%S'),sep='');
#            calibfn=paste(fn,'.calib', sep='');
#            calibpath=file.path(inpath, calibfn)
#            pbsfn=paste(fn,'.pbs',sep='');
#            logfn=paste(fn,'.log',sep='');
#        }        #pbscmd=paste('./pihm', '-c ',calibfn, projectname, fn, '> ', logfn, sep=' ')
        pbscmd=paste('./pihm', '-c ',calibfn, projectname, fn, sep=' ')

        if (if.pbs){
            writepbs(fn=pbsfn,pbscmd,wallhours=wallhours,memory=memory)
            if (!quiet){      
                message('\n\nStart pbs job #', i, '\t@',Sys.time(),'\n');
                message('qsub ',pbsfn);
            }
            syscmd=paste('qsub ',pbsfn, sep=' ')     
        }else{
            syscmd=pbscmd   
        }

        shlines[il*2]=paste('#',i, syscmd);
        shlines[il*2+1]=(syscmd);
        il = il + 1;
        if ( i %% nlines == 0 | i >=nsets){
            write(shlines,file=shfn[ish],append=FALSE);
            ish = ish + 1 
            il=1;
            shlines=character(); 
            shlines[il]='#!/bin/bash'; 
        }
    }
}

writepbs <-function(fn= paste(projectname,'.','pbs',sep=''),cmd,wallhours=24,memory='1bg'){
    pbsfile <- file.path('./',fn);
    pbsstr=c('#PBS -l nodes=1',
           paste('#PBS -l walltime=',wallhours,':00:00',sep=''),
           paste('##PBS -l pmem=',memory,sep=''),
           '#PBS -j oe',
            'cd $PBS_O_WORKDIR',
           cmd
           );
    write.table(pbsstr,pbsfile,col.names=FALSE,row.names=FALSE,quote=FALSE);
    return(fn);
}

generatepbs <-function(fn,wallhours=24,memory='1gb'){
    if(missing(fn)){
        fn= paste('pbs','.',projectname,sep='')
    }         
    pbsfile <- file.path('./',fn);
    pbsstr=c('#PBS -l nodes=1;',
           paste('#PBS -l walltime=',wallhours,':00:00',sep=''),
           paste('#PBS -l pmem=',memory,,sep=''),
           '#PBS -j oe',
            'cd $PBS_O_WORKDIR',
           paste('./pihm -d -v ',projectname)
           );
    write.table(pbsstr,pbsfile,col.names=FALSE,row.names=FALSE,quote=FALSE);
    return(fn);
}

qs <- function(option=paste('-u',system('whoami',,ignore.stdout = FALSE,
                                        intern=TRUE, ignore.stderr = FALSE ) ),
               if.return=FALSE){
    q=qstat(option=option);
    nq=nrow(q);
    message(nq, " Jobs.");
    status <- q[,'S']
    sta<-rep('unkown',length(status));
    sta[which(grepl('C',status))]='Cancelling'
    sta[which(grepl('Q',status))]='Waiting'
    sta[which(grepl('R',status))]='Runing'
    
    usta <- unique(sta);
    for (i in 1:length(usta)){
        numb = sum(sta==usta[i])
        if (numb >0){
            message( numb,' Jobs in ', usta[i], '\n');
        }
    }
    usercol <- q[,'Username'];
    uu=unique(usercol);
    message(length(uu), ' users in queue');
    if(if.return){
        return(q)
    }
    else{
    }
}

qstat <- function(option='' ){
    cmd=paste('qstat ', option, sep='');
    lines <- system(cmd ,ignore.stdout = FALSE, intern=TRUE, ignore.stderr = FALSE);
    #lines=lines[-(c(1:3,5))]
    #head=lines[4];
    head=c("Job ID", "Username", "Queue", "Jobname", "SessID", "NDS", "TSK", "Memory", "Time", "S", "Time")
    lines=lines[-c(1:5)]
    a=as.matrix(read.table(text=lines))
    colnames(a)=head
    return(a);
}
bestgof <- function( outdir='ScnComparison/',bestdir='best', copyfile=TRUE){
    N=ncol(gfactors)
    if (N>300){
        num = 150
    }else{
        num = round(N/3)
    }
    nid = pickbest(key='NSE', decreasing=TRUE, numbers=num,bestdir='best', copyfile=FALSE)
    pid = pickbest(key='PBfdc', decreasing=FALSE, numbers=num,bestdir='best', copyfile=FALSE)
    bestid = pid [pid %in% nid]
   
    
    if (copyfile && length(bestid)>0){
        numbers=length(bestid)
        nfiles=paste('BEST','_',(1:numbers), qffiles[bestid],sep='');
        dir.create(bestdir,showWarnings=FALSE)
        file.copy(paste(outdir, '/',qffiles[bestid],sep=''),
                  paste(bestdir,'/',nfiles,sep=''))
    }

    return(bestid)
}

pickbest <- function(key='NSE', decreasing=TRUE, outdir='ScnComparison/',
                     numbers=10,bestdir='best', copyfile=TRUE){
    var = gfactors[key,]
    if (   grepl('^PBfdc',key)   ){
        bestid=orderdist(var)[1:numbers]
    }else{
        bestid = order(var,decreasing=decreasing)[1:numbers]
    }
    
    if (copyfile){
        nfiles=paste(key,'_',(1:numbers), qffiles[bestid],sep='');
        dir.create(bestdir,showWarnings=FALSE)
        file.copy(paste(outdir, '/',qffiles[bestid],sep=''),
                  paste(bestdir,'/',nfiles,sep=''))
    }
    return(bestid)
}
