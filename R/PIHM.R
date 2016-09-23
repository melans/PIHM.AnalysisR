#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' 
#' =============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#' 
#' =============================================
#' @param  Love PIHM? Defaults to TRUE.
#' @keywords PIHM
#' @export
#' @examples
#' PIHM()

#================================================
#================================================
#================================================
PIHM.path <- function(indir, outdir,pname,minfodir,resdir,pihmver,
                      LAKEON=TRUE, RIVERON=FALSE){
        assign("RIVERON",RIVERON , envir = .GlobalEnv)   #default river network is on.
        assign("LAKEON",LAKEON , envir = .GlobalEnv)   #default lake is off.
 
    if (!missing(indir)){   
        assign("inpath",normalizePath(indir) , envir = .GlobalEnv)   #input of PIHM
    }

    if (!missing(outdir)){ 
        assign("outpath",normalizePath(outdir) , envir = .GlobalEnv)   #output of PIHM
    }
    if (!missing(pname)){
        assign("projectname",pname , envir = .GlobalEnv)
    }
    if (!missing (pihmver) ){
        assign("pihmver",pihmver , envir = .GlobalEnv)
    }
    if (!missing(minfodir)){ 
        assign("ModelInfopath",minfodir , envir = .GlobalEnv)
    }else{
        assign("ModelInfopath",file.path(outpath,'ModelInfo') , envir = .GlobalEnv) #Information of Input files.
    }
    if (!missing(resdir)){ 
        assign("Resultpath",resdir , envir = .GlobalEnv)
    }else{
        assign("Resultpath",file.path(outpath,'PIHMAnalysis') , envir = .GlobalEnv) #ResultPath for analysis.
    }

    if( !file.exists(inpath) ){ dir.create(inpath)};
    if( !file.exists(outpath) ){ dir.create(outpath)};
    if( !file.exists(ModelInfopath) ){ dir.create(ModelInfopath)};
    if( !file.exists(Resultpath) ){ dir.create(Resultpath)};

    
    PIHMdir <- list('inpath'=inpath,
                    'outpath'=outpath,
                    'ModelInfopath'=ModelInfopath, 
                    'Resuiltpath' =Resultpath,
                    'projectname'=projectname,
                    'pihmver' =pihmver,
                    'RIVERON'=RIVERON,
                    'LAKEON'=LAKEON);
    assign("PIHMdir", PIHMdir, envir = .GlobalEnv) 
    return(PIHMdir);
}

quickset <- function(pj){
    fd=list.files('../../input',pattern=paste(pj,'$',sep=''),full.names=TRUE);
    if (length(fd)==1){
        inpath=fd;
    }
    outpath='./'
    projectname=pj;
    
    Sys.setenv(TZ = "UTC")
    PIHM(indir=inpath,outdir=outpath,pname=projectname,pihmver=2.4)
}
 
quickset22 <- function(pj){
    fd=list.files('./',pattern=paste(pj,'$',sep=''),full.names=TRUE);
    if (length(fd)==1){
        inpath=fd;
    }
    outpath='./'
    projectname=pj;
    
    Sys.setenv(TZ = "UTC")
    PIHM(inpath=inpath,outpath=outpath,projectname=projectname,pihmver=2.2)
}

PIHM <-function(inpath='input/', outpath='./',projectname,pihmver=2.2){
    cat ("\n\n");
    cat ("\n\t########  #### ##     ## ##     ##");
    cat ("\n\t##     ##  ##  ##     ## ###   ###"); 
    cat ("\n\t##     ##  ##  ##     ## #### ####");
    cat ("\n\t########   ##  ######### ## ### ##");
    cat ("\n\t##         ##  ##     ## ##     ##");
    cat ("\n\t##         ##  ##     ## ##     ##"); 
    cat ("\n\t##        #### ##     ## ##     ##");
    cat ("\n\t    The Penn State Integrated Hydrologic Model");
    cat ("\n\t    Current version is PIHM-MF and PIHM v2.4");
    cat ("\n\n\n");
    Sys.setenv(TZ = "UTC")
   pihmver=pihmver 
   .timefunc()
    if ( exists('MAX_THREADS') ) {
        MAX_THREADS = MAX_THREADS
    }else{
        MAX_THREADS=as.numeric(system('sysctl -n hw.ncpu',intern=TRUE))*2 
    }
     assign("MAX_THREADS",MAX_THREADS , envir = .GlobalEnv)

   PIHMdir <- PIHM.path(inpath,outpath,projectname,pihmver=pihmver)   
   return(PIHMdir)
}


PIHM.mode <- function(index, mode='analysis'){
    str=c('calib',
                    'init',
                    'para',
                    'mesh',
                    'att',
                    'lc')
    switch=logical(length(str));

    names(switch) <- str; 
    if (grepl('^ana',mode)){
        str='Analysis';
        key=1
        switch=!switch;     #all reading from bakup files.
    }else if(grepl('^calib',mode)){
        str='Calibration';
        key=2
    }else if(grepl('^prepare',mode)){
        str='Prepare';
        key=0
    }else{
        str='unknown';
        key=-1
    }
    modelist<-list('key'=key,'mode'=str,'bak'=switch)
    return(modelist)
}


.timefunc <- function(){
  require('Rcpp') #for time_t to time.
    cppFunction('String  t2time( long int intime) {
        time_t rawtime;
        struct tm *utctime;
        char result[100];
        rawtime=(time_t) intime;
        utctime = gmtime(&rawtime);
        std::sprintf(result, "%4d-%2d-%2d %2d:%2d:%2d",
            1900 + utctime->tm_year,  1 + utctime->tm_mon, utctime->tm_mday,
           utctime->tm_hour,    utctime->tm_min, utctime->tm_sec
            );
        return result;
    }')
    assign('t2time', t2time,envir = .GlobalEnv) 
}
#x11.ready <- function(){
#    a= capabilities();
#    if (a['X11']){
#        return(TRUE);
#    }else{
#        warning('X11 is not ready to use')
#        print(a)
#        return(FALSE);
#    }
#}



