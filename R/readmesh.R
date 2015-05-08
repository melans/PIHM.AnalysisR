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


readmesh <-function(){
    meshfile <- file.path(inpath, paste(projectname,".mesh",sep=''));
    if (pihmver >=2.4 ){
        ncell=scan(meshfile,what=integer(),nmax=1,blank.lines.skip = TRUE,quiet = TRUE);
        mshhead=scan(meshfile,what=character(),nlines=1,blank.lines.skip = TRUE,quiet = TRUE);

        msh <-t( matrix (scan(meshfile,what=integer(),skip=1,nlines=ncell,blank.lines.skip = TRUE,quiet = TRUE), ncol=ncell))

        npt=scan(meshfile,what=integer(),nmax=1,skip=ncell+1,blank.lines.skip = TRUE,quiet = TRUE);
        pthead=scan(meshfile,what=character(),nlines=1,skip=ncell+1,blank.lines.skip = TRUE,quiet = TRUE);
        pt <-t( matrix (scan(meshfile,what=double(),skip=ncell+2,nlines=ncell,blank.lines.skip = TRUE,quiet = TRUE), ncol=npt))

        colnames(msh)= c("ID",mshhead[-1]) 
        colnames(pt)=c("ID",pthead[-1]);
    }
    else{
        num=scan(meshfile,what=integer(),nmax=2,blank.lines.skip = TRUE,quiet = TRUE);
        ncell=num[1];
        npt=num[2];
        msh <-t( matrix (scan(meshfile,what=integer(),skip=1,nlines=ncell,blank.lines.skip = TRUE,quiet = TRUE), ncol=ncell))
        pt <-t( matrix (scan(meshfile,what=double(),skip=ncell+1,nlines=ncell,blank.lines.skip = TRUE,quiet = TRUE), ncol=npt))

        colnames(msh) <- c("ID","NODE1","NODE2","NODE3","NABR1","NABR2","NABR3")
        colnames(pt) <- c("ID", "X","Y","ZMIN","ZMAX");

    }
    mesh <-list("size"=c(ncell,npt),"mesh"=msh, "points"=pt );
    return(mesh);
}
