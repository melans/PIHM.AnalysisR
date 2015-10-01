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
PIHM.path <- function(indir, outdir,pname,minfodir,resdir,ver){
    
    if (!missing(indir)){   
        assign("inpath",normalizePath(indir) , envir = .GlobalEnv)   #input of PIHM
    }

    if (!missing(outdir)){ 
        assign("outpath",normalizePath(outdir) , envir = .GlobalEnv)   #output of PIHM
    }
    if (!missing(pname)){
        assign("projectname",pname , envir = .GlobalEnv)
    }
    if (!missing (ver) ){

        assign("pihmver",ver , envir = .GlobalEnv)
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

    
    PIHMdir <- list(inpath,outpath,  projectname, ModelInfopath, Resultpath,pihmver);
    names(PIHMdir) <- as.character(c( 'inpath','outpath',  'projectname', 'ModelInfopath', 'Resultpath','version'));
    assign("PIHMdir", PIHMdir, envir = .GlobalEnv) 
    return(PIHMdir);
}

quickset <- function(pj){
    fd=list.files('../../input',pattern=pj,full.names=TRUE);
    if (length(fd)==1){
        inpath=fd;
    }
    outpath='./'
    projectname=pj;
    
    Sys.setenv(TZ = "UTC")
    PIHM(indir=inpath,outdir=outpath,pname=projectname,ver=2.4)
}
 
PIHM <-function(indir, outdir,pname,ver){
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
   pihmver=2.4 
    loadinglib();
   .timefunc()
   if ( !exists('inpath') ) {
       if (missing(indir) && !exists('inapth')  ){
           indir <- readline(prompt="Path of input folder for PIHM(ENTER = current word directory):\n");
           while (nchar(indir)<=0 ) {
               indir <- './' #readline(prompt="Wrong input. Let's try again.\n path of input folder for PIHM:\n");
           }
           while (!file.exists(indir)  ) {
               indir <- readline(prompt="Folder doesn't exist. Try again. \n path of input folder for PIHM:\n");
           }
       }
   }else{ indir=inpath;}

   if (missing(outdir) ){
       if ( !exists('outpath') ) { 
           outdir <- readline(prompt="Path of output folder for PIHM(ENTER if same as above):\n");
           if (nchar(outdir)<=0){
               outdir <- indir
           }
           while (!file.exists(outdir)  ) {
               outdir <- readline(prompt="Folder doesn't exist. Try again. \n path of input folder for PIHM:\n");
           }
       }else{
           outdir=outpath;
        
       }
    }

    if ( !exists('projectname') ) {
        if (missing(pname) ){
            pname <- readline(prompt="Your project name plese:\n");
            while (nchar(pname)<=0 ) {
                pname <- readline(prompt="path of input folder for PIHM:\n");
            }
        }
    }else{ pname=projectname;}

    if ( !exists('pihmver') ) {
        ver=2.4
        if (missing(ver) ){
            ver <- readline(prompt="Version of your PIHM(2.0, or 2.4(PIHM-MF). Default=2.4)\n");
        }
        
        if (nchar(ver)<=0 ){
            ver <- 2.4; 
        }else {
            if ( ver >2.4 || ver < 2.0) {
                ver <- 2.4; 
            }
        }
    }else{ ver=pihmver;}

    if ( exists('MAX_THREADS') ) {
        MAX_THREADS = MAX_THREADS
    }else{
        MAX_THREADS=as.numeric(system('sysctl -n hw.ncpu',intern=TRUE))*2 
    }
     assign("MAX_THREADS",MAX_THREADS , envir = .GlobalEnv)


   PIHMdir <- PIHM.path(indir,outdir,pname,ver=ver)   
   PIHM.path();

#    pihmin <- loadinput();
#   assign("PIHMIN",pihmin , envir = .GlobalEnv)  
    cat ("\n\n");
    return(PIHMdir);
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
x11.ready <- function(){
    a= capabilities();
    if (a['X11']){
        return(TRUE);
    }else{
        return(FALSE);
    }
}
