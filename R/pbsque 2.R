#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
pbsPIHM.sensitivity  <-
    function(wallhours=24,memory='1gb',
             quiet=TRUE,shnum=1, if.pbs=TRUE,RDSfile){
    #shnum -- number of shell files.
    # if.pbs --- if run the calibration on PBS. default=TRUE. 
    trials <- readtrial();
    initcalib <- readcalib(bak=FALSE);

    intrials = unlist(lapply(trials, function(x) length(x)))
    idpara =which(intrials>1);   #id of parameters in sensitivity test
    paranames= names(intrials[idpara])
    npara =length(idpara) #number of parameters in sensitivity test.
  
    shfn=paste('pbs',seq(1,npara,by=1),'_',paranames,'.sh',sep='');
    shlines=character(); 
    triallen <- do.call(rbind, lapply(trials, function(x) length(x))) 
    nsets=sum(triallen[which(triallen>1)])

           shlines[1]='#!/bin/bash';
    ish=1;
    il = 1; # control the line number of shlines.
    
   message('\n\n',nsets,' trials are waiting for resources. \n', npara,' shell files will be writen\n');
   if(!quiet){
       message('Go on ? Yes/(No) ');
       line <- readline()
       if ( !grepl('^y', tolower(line)) ){
           stop(' Abort. \n');
       }
   }

   message('\n\nStart calibration processes\t',Sys.time(),'\n');

    saveRDS(file=paste('Sensitivity.', strftime(Sys.time(),format='%y%m%d', tz='UTC'),'.RDS',sep=''), trials);
    iset=1

    for (ip in 1:npara){    #loop for each parameter in .trial file.
        pname=paranames[ip] 
        paravalues= unlist(trials[pname])
        nvalue = length(paravalues) #number of value in this parameters.
        pbslines=character(nvalue);

        calib=initcalib;    #reinitial calib value.
        cat('\n',ip,pname,'=');
        for (ivalue in 1:nvalue){
            cat('\t',paravalues[ivalue])
            calib$value[pname] = paravalues[ivalue]

            num=formatC(iset, width = ceiling(log10(nsets)), format = "d", flag = "0")
            fn=paste(projectname,'_',num,'.',pname,'=',round(paravalues[ivalue],3) ,'_', strftime(Sys.time(),format='%y%m%d', tz='UTC'),sep='');
            calibfn=paste(fn,'.calib', sep='');
            calibpath=file.path(inpath, calibfn)
                Sys.sleep(0.01)

            writecalib(calib,newfilename=calibfn);
            pbsfn=paste(fn,'.pbs',sep='');
            logfn=paste(fn,'.log',sep='');
        
            pbscmd=paste('./pihm', '-c ',calibfn, projectname, fn, sep=' ')

            if (if.pbs){
                writepbs(fn=pbsfn,pbscmd,wallhours=wallhours,memory=memory)
                if (!quiet){      
                    message('\n\nStart pbs job #', iset, '\t@',Sys.time(),'\n');
                    message('qsub ',pbsfn);
                }
                syscmd=paste('qsub ',pbsfn, sep=' ')     
            }else{
                syscmd=pbscmd   
            }

            shlines[il*2]=paste('#',iset, syscmd);
            shlines[il*2+1]=(syscmd);
            il = il + 1;
            iset=iset+1   

        }
        write(shlines,file=shfn[ish],append=FALSE);
        ish = ish + 1 
        il=1;
        shlines=character(); 
        shlines[il]='#!/bin/bash'; 
        
        message('');

    }
    write(paste('./',shfn,sep=''), file='all.sh',append=FALSE);
}
pbsPIHM.LoadAllData <-function(workingdir='./', path='alldata'){
    if( !file.exists(path) ){ dir.create(path)};
    projlist <- dir( path = workingdir, pattern = pattern,full.names=TRUE);
    projdir <- dir( path = workingdir, pattern = pattern,full.names=FALSE);
    rdsfiles = paste(workingdir, '.RDS',sep='');
    rdscopy = paste(path, '.RDS',sep='');

    np <- length(projlist)

    for (i in 1:np){
        assign("outpath",normalizePath(projlist[i]) , envir = .GlobalEnv) 
        loadoutput(path=projlist[i], rdsname=rdsfiles[i])
        file.rename(from=rdsfiles[i], to=rdscopy[i])
    }
}
