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
    if (pihmver > 2.3){
        forcdata <- readforcmf();
    }else{
    }

    return(forcdata);
}

#==================================================
#==========read forc for MF version ===============
#==================================================
readforcmf <- function(){ 
        forcfile=list.files(inpath,pattern=paste(projectname,".forc",sep=""),full.names=TRUE);
    if (!file.exists(forcfile)){
        stop("Error: file does not exist\n\t",forcfile, "\n");
    }
    moveon=0;  
    lines <- readLines(forcfile);
   # message('\nFinish loading of .forc file')
    rid <- which(grepl('^num_meteo',tolower(lines)))
    NumMeteoTS <- as.numeric(unlist(strsplit(lines[rid],' +'))[2]);
    
    rids <- which(grepl('^meteo_ts',tolower(lines)))
    Forcing <- list();
    for (i in 1:NumMeteoTS){
        message('Parsing ',i,'/',NumMeteoTS,' sites\n');
        str <- unlist(strsplit(lines[rids[i]],' +'));
        mid <- as.numeric(str[2]);
        windHeight <- as.numeric(str[4])
        if (i< NumMeteoTS && length(rids)>1 ){
            sub <- lines[rids[i]+3:rids[i+1]-1]
        }else{
            sub <- lines[rids[i]+3:length(lines)]
        } 
        tmplist <- scan(text = sub[!is.na(sub)] , what=list('character','character','numeric','numeric','numeric','numeric','numeric','numeric','numeric'),quiet = TRUE)
        t <- as.POSIXct(paste(unlist(tmplist[[1]]),unlist(tmplist[[2]])),format='%Y-%m-%d %H:%M',tz='UTC');
        matdata <- matrix(as.numeric(unlist(tmplist[3:length(tmplist)])),ncol=7);
        
        ts <- xts(matdata,order.by = t);
        Forcing[[i]] <- list("ID" = mid, "WindHeight" = windHeight, "ts" = ts);
    }
    return(forc)
}
#==================================================
#       read forc for 2.0 version 
#==================================================
