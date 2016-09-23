loadinglib <- function( 
        liblist=c(
                  'xts', #for time-series.
            'geometry'# for ploting 3D.
            ,'akima'
            ,'hydroGOF'#good of fit for Hydrograph
            #'quantmod',
           # 'Rcpp', #for time_t to time.
            #'EGRET'            ,'dataRetrieval' #for downloading USGS data
          #  ,'tripack'
          #  ,'rgl'  # for 3D plot.
            #,'rgeos'
            ,'hydroTSM'  #Hydro TS management.
            ,'lubridate'
            #,'EcoHydRology'
            , 'lattice'       # for levelplot, image.plot
            ,'fields'
            ,'plotrix'
            ,'data.table'
            ,'sp'   #spatial data GIS
            ,'maptools' #map tool
            ,'raster'
            , 'aod'
            ),
              quiet=TRUE      ){
    Sys.setenv(TZ = "UTC")

  for (i in 1:length(liblist)){
      libname <- liblist[i];
      if(require(libname,character.only=TRUE)){
          if (!quiet){
            message(libname," is loaded correctly.")
          }
      } else {
          message("... \ttrying to install ",libname)
          install.packages(libname, repos='https://cran.cnr.berkeley.edu/')
          require(libname)
          if(require(libname)){
              message(libname," installed and loaded.")
          } else {
              warning("Could not install ",libname)
          }
      }
  }
}
loadinglib.CC <- function(quiet=TRUE, 
        liblist=c(#'xts', #for time-series.
            'geometry'# for ploting 3D.
            ,'akima'
            ,'hydroGOF'#good of fit for Hydrograph
            #'quantmod',
           # 'Rcpp', #for time_t to time.
            #'EGRET'            ,'dataRetrieval' #for downloading USGS data
          #  ,'tripack'
            ,'rgl'
            #,'rgeos'
            ,'hydroTSM'  #Hydro TS management.
            ,'lubridate'
            #,'EcoHydRology'
            ,'fields'
            ,'plotrix'
            ,'data.table'
            ,'RNetCDF'  #netcdf surpport
            ,'RCMIP5'
            )
                    ){
    Sys.setenv(TZ = "UTC")

  for (i in 1:length(liblist)){
      libname <- liblist[i];
      if(require(libname,character.only=TRUE)){
          if (!quiet){
            message(libname," is loaded correctly.")
          }
      } else {
          message("... \ttrying to install ",libname)
          install.packages(libname, repos='http://cran.us.r-project.org')
          if(require(libname)){
              message(libname," installed and loaded.")
          } else {
              warning("Could not install ",libname)
          }
      }
  }
}

scmip5<-function(){
    cmip5dir=paste(sourcedir,'/CMIP5',sep='')
    s=list.files(cmip5dir,pattern="*.R",full.names=TRUE)
    if(length(s)<=0){
        stop('error when loading CMIP5 source code')
    }
    for (i in 1:length(s)){
        cat(i,"\t",s[i],"\n") ;  
        source(s[i]) 
    }
    cat('done\n');
    loadinglib.CC();
    return(cmip5dir);
}
