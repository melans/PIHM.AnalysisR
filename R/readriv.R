#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 10:53:00 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;
#' 
#' 
#'  <- ============================================
#' @param  Path of output folder.
#' @param  prpjectname.
#' @keywords read input. mesh file.
#' @export  List of river data. river
#' @examples
#' readriv()


readriv <-function(){
    rivfile <- file.path(inpath, paste(projectname,".riv",sep=''));
    moveon =0 ;
    lines <- readLines(rivfile);
# riv
    nriv=scan(rivfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon );
    rivhead=scan(rivfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
    riv <-t( matrix (scan(rivfile,what=integer(),nlines=nriv,blank.lines.skip = TRUE,skip=moveon+1), ncol=nriv))
    outlets=which(riv[,4]<0);
#Shape
    moveon=which(tolower(lines) == "shape")
    nshp=scan(rivfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon);
    shphead=scan(rivfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
    shp <-t( matrix (scan(rivfile,what=numeric(),nlines=nshp,blank.lines.skip = TRUE,skip=moveon+1), ncol=nshp))
    

#Materials
    moveon=which(tolower(lines) == "material")
    nmat=scan(rivfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon);
    mathead=scan(rivfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
    mat <-t( matrix (scan(rivfile,what=numeric(),nlines=nmat,blank.lines.skip = TRUE,skip=moveon+1), ncol=nmat))
    

#IC
    moveon=which(tolower(lines) == "ic")
    nic=scan(rivfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon);
    ichead=scan(rivfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
    ic <-t( matrix (scan(rivfile,what=numeric(),nlines=nic,blank.lines.skip = TRUE,skip=moveon+1), ncol=nic))
    
#BC
#    moveon=which(tolower(lines) == "bc")
#    nbc=scan(rivfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon);
#    bchead=scan(rivfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
#    bc <-t( matrix (scan(rivfile,what=numeric(),nlines=nbc,blank.lines.skip = TRUE,skip=moveon+1), ncol=nbc))
    if (pihmver>=2.4){
        colnames(riv)	=	 c("ID",rivhead[-1]) 
        colnames(shp)	=	 c("ID",shphead[-1]) 
        colnames(mat)	=	 c("ID",mathead[-1]) 
        colnames(ic)	=	 c("ID",ichead[-1]) 
#        colnames(bc)	=	 c("ID",bchead[-1]) 
    }else{
        colnames(riv)	=	c("ID","FROM","TO","DOWN","LEFT","RIGHT","SHAPE","MATERIAL","IC","BC","RES")
        colnames(shp)	=	c("ID","RIVDPTH","O_INT","C_WID")
        colnames(mat)	=	c("ID","RIV_ROUGH","CWR","RIVHK","RIVVK","BEDTHICK_CAL")
        colnames(ic)	=	c("ID","HRIV")
#        colnames(bc)	=	
    }
    material <-list("size"=c(nmat),"mat"=mat);
    shape    <-list("size"=c(nshp),"shp"=shp );
    riverseg <-list("size"=c(nriv),"riv"=riv, "outlets"=outlets);
    InitCond <-list("size"=c(nic),"bc"=ic);
#   BoundCond <-list("size"=c(nbc),"bc"=bc);
RivInfo <-list("River"=riverseg, "Shape"=shape, "Material"=material, "IC"=InitCond);
    
    return(RivInfo);
}
