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

PIHM.path <- function(indir, outdir,pname,minfodir,resdir,ver ){
    m=0;
    if (nargs() <1){

        cat("\nProject Name=\t")   ;
        if( exists("projectname" ) )
        { cat(projectname,"\n"); } 
        else{ cat('MISSING\n');m=1}
        cat("\nPIHM Version=\t")   ;
        if( exists("pihmver" ) )
        { cat(pihmver,"\n"); } 
        else{ cat('MISSING\n');m=2}

        
        cat("\nInput folder of PIHM=\n\t") 
        if( exists("inpath" ) ){ cat(inpath,"\n");} 
        else {cat('MISSING\n');m=3 }

        cat("\nOutput folder of PIHM=\n\t") 
        if( exists("outpath" ) ){ cat(outpath,"\n");}
        else {cat('MISSING\n');m=4}
        
        cat("\nInformation of PIHM input=\n\t") 
        if( exists("ModelInfopath" ) ){ cat(ModelInfopath,"\n"); }
        else {cat('MISSING\n');m=5}
        
        cat("\nOutput folder of Analysis Tool=\n\t") 
        if( exists("Resultpath" ) ){ cat(Resultpath,"\n");} 
        else {cat('MISSING\n'); m=6}
        
        if (m>0){
            cat('\n');
#            cat('function: PIHM.path(indir,outdir,pname,minfodir,resdir\n');
#            cat('Example 1: PIHM.path(indir=\'input/example/\',output=\'output/example\',pname=\'example\') \n');
#            cat('Example 2: PIHM.path(minfodir=\'~/ModelInfo/\',resdir=\'/AnalysisDir\' \n');
#           cat('Example 3: PIHM.path(ver=2.0);            
        }
        return(0);
    } 
    
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
        cat(inpath,'\n')
        assign("ModelInfopath",file.path(inpath,'ModelInfo') , envir = .GlobalEnv) #Information of Input files.
    }
    if (!missing(resdir)){ 
        assign("Resultpath",resdir , envir = .GlobalEnv)
    }else{
        assign("Resultpath",file.path(outpath,'PIHMAnalysis') , envir = .GlobalEnv) #ResultPath for analysis.
    }
    if( !file.exists(ModelInfopath) ){ dir.create(ModelInfopath)};
    if( !file.exists(Resultpath) ){ dir.create(Resultpath)};
    PIHMdir <- list(inpath,outpath,  projectname, ModelInfopath, Resultpath,pihmver);
    names(PIHMdir) <- as.character(c( 'inpath','outpath',  'projectname', 'ModelInfopath', 'Resultpath','version'));
    assign("PIHMdir", PIHMdir, envir = .GlobalEnv) 
    return(PIHMdir);
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

   if ( !exists('outpath') ) { 
       if (missing(outdir) ){
           outdir <- readline(prompt="Path of output folder for PIHM(ENTER if same as above):\n");
           if (nchar(outdir)<=0){
               outdir <- indir
           }
           while (!file.exists(outdir)  ) {
               outdir <- readline(prompt="Folder doesn't exist. Try again. \n path of input folder for PIHM:\n");
           }
       }
   }else{ outdir=outpath;}

    if ( !exists('projectname') ) {
        if (missing(pname) ){
            pname <- readline(prompt="Your project name plese:\n");
            while (nchar(pname)<=0 ) {
                pname <- readline(prompt="path of input folder for PIHM:\n");
            }
        }
    }else{ pname=projectname;}

    if ( !exists('pihmver') ) {
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


  liblist=c('xts', #for time-series.
            'geometry',# for ploting 3D.
            'akima',
            'hydroGOF',#good of fit for Hydrograph
            'quantmod',
            'Rcpp', #for time_t to time.
            'EGRET','dataRetrieval' #for downloading USGS data
            );
      
  
  for (i in 1:length(liblist)){
      libname <- liblist[i];
      if(require(libname,character.only=TRUE)){
          message(libname," is loaded correctly.")
      } else {
          message("... \ttrying to install ",libname)
          install.packages(libname, dep=TRUE,repos='http://cran.us.r-project.org')
          if(require(libname)){
              message(libname," installed and loaded.")
          } else {
              stop("Could not install ",libname)
          }
      }
  }
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


   PIHMdir <- PIHM.path(indir,outdir,pname,ver=ver)   
   PIHM.path();
#    pihmin <- loadinput();
#   assign("PIHMIN",pihmin , envir = .GlobalEnv)  

    cat ("\n\n");
    return(PIHMdir);
}


