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
pbstrial  <- function(pbsfile){
    if(missing(pbsfile)){
        pbsfile=generatepbs();
    }
    trials <- readtrial();
    calib <- readcalib();
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
writepbs <-function(fn,cmd){
    if(missing(fn)){
        fn= paste('pbs','.',projectname,sep='')
    }         
    pbsfile <- file.path('./',fn);
    pbsstr=c('#PBS -l nodes=1;',
           '#PBS -l walltime=24:00:00',
           '#PBS -j oe',
            'cd $PBS_O_WORKDIR',
           cmd
           );
    write.table(pbsstr,pbsfile,col.names=FALSE,row.names=FALSE,quote=FALSE);
    return(fn);
}

generatepbs <-function(fn){
    if(missing(fn)){
        fn= paste('pbs','.',projectname,sep='')
    }         
    pbsfile <- file.path('./',fn);
    pbsstr=c('#PBS -l nodes=1;',
           '#PBS -l walltime=24:00:00',
           '#PBS -j oe',
            'cd $PBS_O_WORKDIR',
           paste('./pihm -d -v ',projectname)
           );
    write.table(pbsstr,pbsfile,col.names=FALSE,row.names=FALSE,quote=FALSE);
    return(fn);
}

pbswritecalib <- function(calib){
    fn= paste(projectname,".",'calib.',as.character(Sys.time()),sep='');
    theFile <- file.path(inpath, fn);
    i=0;
    while( exists(theFile)){
        fn=paste(projectname,".",'calib.',as.character(Sys.time()),'_',sep='')
        theFile<- file.path(inpath, fn);
    }
    file.create(file=theFile);
    for (i in 1:length(calib$offon) ) {

        if (calib$offon[i]){
            str = paste(names(calib$value[i]),calib$value[i],calib$comments[i],sep='\t') ;
        }else{
            str = paste(paste('#',names(calib$value[i]),sep=''),calib$value[i],calib$comments[i], sep='\t')
        }
            write(x=str,file=theFile,append =TRUE)
    }
    return(fn);
}
