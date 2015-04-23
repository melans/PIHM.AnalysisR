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


readriv <-function(inpath="./",projectname=0,version=2.2){
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
    forcfile=paste(inpath,projectname,".forc",sep="");
    
    if (!file.exists(forcfile)){
        cat("Error: file does not exist\n\t",paste(riverfile), "\n");
        return(0);
    }
    
    
    moveon=0;   #number of skip lines.
    lines <- readLines(forcfile);
#forc head 
    forcfile <- paste(inpath, projectname,".riv",sep='');
    nhead=scan(forcfile,what=integer(),blank.lines.skip = TRUE,nlines=1, skip=moveon );
        
#Shape
    moveon=which(tolower(lines) == "shape")
    nshp=scan(forcfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon);
    shphead=scan(forcfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
    shp <-t( matrix (scan(forcfile,what=numeric(),nlines=nshp,blank.lines.skip = TRUE,skip=moveon+1), ncol=nshp))
    shape <-list("size"=c(nshp),"shp"=shp, "headerMesh"=shphead);

#Materials
    moveon=which(tolower(lines) == "material")
    nmat=scan(forcfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon);
    mathead=scan(forcfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
    mat <-t( matrix (scan(forcfile,what=numeric(),nlines=nmat,blank.lines.skip = TRUE,skip=moveon+1), ncol=nmat))
    material <-list("size"=c(nmat),"mat"=mat, "headerMesh"=mathead);

#IC
    moveon=which(tolower(lines) == "ic")
    nic=scan(forcfile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon);
    ichead=scan(forcfile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
    ic <-t( matrix (scan(forcfile,what=numeric(),nlines=nic,blank.lines.skip = TRUE,skip=moveon+1), ncol=nic))
    InitCond <-list("size"=c(nic),"ic"=ic, "headerMesh"=ichead);

RivInfo <-list("River"=riverseg, "Shape"=shape, "Material"=material, "IC"=InitCond);
    
    return(RivInfo);
}
