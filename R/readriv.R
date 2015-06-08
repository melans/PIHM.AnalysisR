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


readriv <-function(bak=FALSE){
    if (bak){
        theFile <- list.files(path=outpath, pattern=paste(projectname,".",'riv.bak',sep=''),full.names=TRUE);
        if (length(theFile)<=0){
            warning('The riv file in input folder was read, instead of in output folder.\n');
            theFile <- list.files(path=inpath, pattern=paste(projectname,".",'riv$',sep=''),full.names=TRUE);
        }   
    }else{
        theFile <- list.files(path=inpath, pattern=paste(projectname,".",'riv$',sep=''),full.names=TRUE);
    }

    if (!file.exists(theFile)){
        stop ("\n\n\n file \'", theFile , "\' is missing\n\n");
    }
    theFile <- file.path(inpath, paste(projectname,".riv",sep=''));
    moveon =0 ;
    lines <- readLines(theFile);
# riv
    nriv=scan(theFile,what=integer(),nmax=1,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon );
    rivhead=scan(theFile,what=character(),nlines=1,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon);
    riv <-t( matrix (scan(theFile,what=integer(),nlines=nriv,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon+1), ncol=nriv))
    outlets=which(riv[,4]<0);
#Shape
    moveon=which(tolower(lines) == "shape")
    nshp=scan(theFile,what=integer(),nmax=1,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon);
    shphead=scan(theFile,what=character(),nlines=1,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon);
    shp <-t( matrix (scan(theFile,what=numeric(),nlines=nshp,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon+1), ncol=nshp))

#Materials
    moveon=which(tolower(lines) == "material")
    nmat=scan(theFile,what=integer(),nmax=1,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon);
    mathead=scan(theFile,what=character(),nlines=1,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon);
    mat <-t( matrix (scan(theFile,what=numeric(),nlines=nmat,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon+1), ncol=nmat))
    

#IC
    moveon=which(tolower(lines) == "ic")
    nic=scan(theFile,what=integer(),nmax=1,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon);
    ichead=scan(theFile,what=character(),nlines=1,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon);
    ic <-t( matrix (scan(theFile,what=numeric(),nlines=nic,blank.lines.skip = TRUE,quiet = TRUE,skip=moveon+1), ncol=nic))
    
#BC
#    moveon=which(tolower(lines) == "bc")
#    nbc=scan(theFile,what=integer(),nmax=1,blank.lines.skip = TRUE,skip=moveon);
#    bchead=scan(theFile,what=character(),nlines=1,blank.lines.skip = TRUE,skip=moveon);
#    bc <-t( matrix (scan(theFile,what=numeric(),nlines=nbc,blank.lines.skip = TRUE,skip=moveon+1), ncol=nbc))
    if (pihmver>=2.4){
        colnames(riv)	=	toupper( c("ID",rivhead[-1]) ) 
        colnames(shp)	=	toupper( c("ID",shphead[-1]) )
        colnames(mat)	=	toupper( c("ID",mathead[-1]) )
        colnames(ic)	=	toupper( c("ID",ichead[-1]) )
#        colnames(bc)	=	toupper( c("ID",bchead[-1]) )
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

    #====segment length ============
    fr<-riv[,2];
    to<-riv[,3];
    pts<-readmesh()$points;
    pt1<-cbind(pts[fr,2],pts[fr,3]);
    pt2<-cbind(pts[to,2],pts[to,3]);
    dist<- Eudist(pt1,pt2);
#========Surface Area==============
    surfA <- shp[riv[,'SHAPE'],4] * dist;

RivInfo <-list("River"=riverseg, "Shape"=shape, "Material"=material, "IC"=InitCond,"segLength"=dist,'surfArea'=surfA);
    
    return(RivInfo);
}
    
