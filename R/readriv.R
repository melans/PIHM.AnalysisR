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


readriv <-function(inpath="./",projectname=0){
    nargin <- nargs();
    if (nargin <1){
        print("\nUsage:\n\t readriv <-function(inpath=\"./\",projectname)\n");
        print("\n\n");
        return(0);
    }
    
    if (substring(inpath,nchar(inpath))=="/"){
    }else{
        inpath <- paste(inpath,"/",sep='');    
    }
    if (projectname==0){ # default: projenctname can be access from projectName.txt;
        projectname=scan(paste(inpath,"projectName.txt",sep=''));
    }
    rivfile=paste(inpath,projectname,".riv",sep="");
    if (!file.exists(rivfile)){
        cat("Error: file does not exist\n\t",paste(riverfile), "\n");
        return(0);
    }

    moveon=0;   #number of skip lines.
    lines <- readLines(rivfile);
# riv
    rivfile <- paste(inpath, projectname,".riv",sep='');
    nriv=scan(rivfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon );
    rivhead=scan(rivfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
    riv <-t( matrix (scan(rivfile,what=integer(),nlines=nriv,blank.lines.skip = TRUE,skip=moveon+1), ncol=nriv))
    outlets=which(riv[,4]<0);
    riverseg <-list("size"=c(nriv),"riv"=riv, "headerMesh"=rivhead,"outlets"=outlets);
    
#Shape
    moveon=which(tolower(lines) == "shape")
    nshp=scan(rivfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon);
    shphead=scan(rivfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
    shp <-t( matrix (scan(rivfile,what=numeric(),nlines=nshp,blank.lines.skip = TRUE,skip=moveon+1), ncol=nshp))
    shape <-list("size"=c(nshp),"shp"=shp, "headerMesh"=shphead);

#Materials
    moveon=which(tolower(lines) == "material")
    nmat=scan(rivfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon);
    mathead=scan(rivfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
    mat <-t( matrix (scan(rivfile,what=numeric(),nlines=nmat,blank.lines.skip = TRUE,skip=moveon+1), ncol=nmat))
    material <-list("size"=c(nmat),"mat"=mat, "headerMesh"=mathead);

#IC
    moveon=which(tolower(lines) == "ic")
    nic=scan(rivfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon);
    ichead=scan(rivfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
    ic <-t( matrix (scan(rivfile,what=numeric(),nlines=nic,blank.lines.skip = TRUE,skip=moveon+1), ncol=nic))
    InitCond <-list("size"=c(nic),"ic"=ic, "headerMesh"=ichead);

RivInfo <-list("River"=riverseg, "Shape"=shape, "Material"=material, "IC"=InitCond);
    
    return(RivInfo);
}
