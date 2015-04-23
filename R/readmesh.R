#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' @param  inpath Path of output folder.
#' @param  prpjectname.
#' @keywords read input. mesh file.
#' @return A list. mesh$mesh, mesh$points, mesh$size etc
#' @export  List of mesh data.
#' @examples
#' readmesh(inpath="./",projectname)


readmesh <-function(inpath="./",projectname){
    nargin <- nargs();
    if (nargin <1){
        cat("\nUsage:\n\t readmesh <-function(inpath=\"./\",projectname)\n");
        cat("\n\n");
        return(0);
    }
    
    if (substring(inpath,nchar(inpath))=="/"){
    }else{
        inpath <- paste(inpath,"/",sep='');    
    }
    if (nargin <2){ # default: projenctname can be access from projectName.txt;
        projectname=scan(paste(inpatth,"projectName.txt",sep=''));
    }

meshfile <- paste(inpath, projectname,".mesh",sep='');
ncell=scan(meshfile,what=integer(),nmax=1,blank.lines.skip = TRUE);
mshhead=scan(meshfile,what=character(),nlines=1,blank.lines.skip = TRUE);

msh <-t( matrix (scan(meshfile,what=integer(),skip=1,nlines=ncell,blank.lines.skip = TRUE), ncol=ncell))

npt=scan(meshfile,what=integer(),nmax=1,skip=ncell+1,blank.lines.skip = TRUE);
pthead=scan(meshfile,what=character(),nlines=1,skip=ncell+1,blank.lines.skip = TRUE);
pt <-t( matrix (scan(meshfile,what=double(),skip=ncell+2,nlines=ncell,blank.lines.skip = TRUE), ncol=npt))

mesh <-list("size"=c(ncell,npt),"mesh"=msh, "headerMesh"=mshhead,"points"=pt, "headerPt"=pthead);
return(mesh);
}
