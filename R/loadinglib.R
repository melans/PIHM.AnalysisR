loadinglib <- function(quiet=TRUE){
        liblist=c(#'xts', #for time-series.
            'geometry',# for ploting 3D.
            'akima',
            'hydroGOF',#good of fit for Hydrograph
            #'quantmod',
           # 'Rcpp', #for time_t to time.
            'EGRET','dataRetrieval' #for downloading USGS data
          #  ,'tripack'
            ,'rgl'
            ,'rgeos'
            ,'hydroTSM'  #Hydro TS management.
            ,'lubridate'
            ,'EcoHydRology'
            ,'fields'
            ,'plotrix'
            ); 
  
    Sys.setenv(TZ = "UTC")

  for (i in 1:length(liblist)){
      libname <- liblist[i];
      if(require(libname,character.only=TRUE)){
          if (!quiet){
            message(libname," is loaded correctly.")
          }
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
}

