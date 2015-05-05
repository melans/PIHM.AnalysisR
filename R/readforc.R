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
#' readforc()


readforc <-function(){
    nargin <- nargs();
    if (nargin <1){
        print("\nUsage:\n\t readriv <-function(inpath=\"./\",projectname)\n");
        print("\n\n");
        return(0);
    }
    

    forcfile=list.files(inpath,pattern=paste(projectname,".forc",sep=""),full.names=TRUE);
    
    if (!file.exists(forcfile)){
        stop("Error: file does not exist\n\t",forcfile, "\n");
    }
    
 #number of skip lines.  
    if (pihmver > 2.3){

    }else{
    }

    readforcmf <- function(){ 
        moveon=0;  
        lines <- readLines(forcfile);
        rid <- which(grepl('^num_meteo',tolower(lines)))
        NumMeteoTS <- as.numeric(unlist(strsplit(lines[rid],' +'))[2]);
        
        rids <- which(grepl('^meteo_ts',tolower(lines)))
        Forcing <- list();
        for (i in 1:NumMeteoTS){
            str <- unlist(strsplit(lines[rids[i]],' +'));
            mid <- as.numeric(str[2]);
            windHeight <- as.numeric(str[4])
            if (i< NumMeteoTS){
                matstr <- as.matrix(lines[rids[i]+3:rids[i+1]-1]);
            }else{
                matstr <- as.matrix(lines[rids[i]+3:length(lines)]);
            }
            t <- as.Date(matstr[,1]);
            matdata <- as.numeric(matstr[,-1]);
            ts <- xts(matdata,order.by = t);
            Forcing[[i]] <- list("ID" = mid, "WindHeight" = windHeight, "ts" = ts);
        }
    }
#forc head 
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
