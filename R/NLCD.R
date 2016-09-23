#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 10:53:00 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;
#' 

NLCD2lc <- function (years=1979:2016){
    att=readatt()
    lc= att[,4];
        message('LC  = ')
    ulc=sort(unique(att[,4]))
        print( ulc)
    if (length( which (ulc <10 | ulc >100) ) ){
        stop('Current LC code is not NLCD')
    }
    # VEGPRMT.TBL
    veg = fun.vegtable(lc =ulc )
    if (pihmver ==2.4){
        writeveg(path=inpath,fn=paste0(projectname,'.lc'), x=veg)
    }else{
        writeveg(path=dirname(inpath), x=veg)
    }

    # att file
    for (i in 1:length(ulc)){
         key = ulc[i];
        lc[which(lc == key) ]= i
    }
    att[,'LC']= lc
    att[,'LAI']=lc
    writeatt(att)
    
    #.lai FILE.
    lr = fun.lairl(lc=ulc,years=years)
    writelai(x=lr)
}
