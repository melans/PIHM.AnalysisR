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
        forcing <- readforcmf();
    }else{
    }
    

    return(forcing);
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
    a <-scan(text =lines[rid], what=list('character','numeric'), quiet = TRUE)  
    NumMeteoTS <- as.numeric(a[2]);
    
    rids <- which(grepl('^meteo_ts',tolower(lines)))
    data <- list();
     
    prcp <- list();
    temp <- list();
    rh <- list()
    winds <- list();
    solar <- list();
    longw <- list();
    press <- list();
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
        tmplist <- scan(text = sub[!is.na(sub)] ,
                        what=list('character','character',
                                  'numeric','numeric','numeric','numeric','numeric','numeric','numeric'),
                        quiet = TRUE)
        t <- as.POSIXct(paste(unlist(tmplist[[1]]),unlist(tmplist[[2]])),format='%Y-%m-%d %H:%M',tz='UTC');
#        matdata <- matrix(as.numeric(unlist(tmplist[3:length(tmplist)])),ncol=7);
        prcp[[i]]   <- xts(as.numeric(unlist(tmplist[3])),order.by = t);
        temp[[i]]   <- xts(as.numeric(unlist(tmplist[4])),order.by = t);
        rh[[i]]     <- xts(as.numeric(unlist(tmplist[5])),order.by = t);
        winds[[i]]  <- xts(as.numeric(unlist(tmplist[6])),order.by = t);
        solar[[i]]  <- xts(as.numeric(unlist(tmplist[7])),order.by = t);
        longw[[i]]  <- xts(as.numeric(unlist(tmplist[8])),order.by = t);
        press[[i]]  <- xts(as.numeric(unlist(tmplist[9])),order.by = t);
        #data[[i]] <- list("ID" = mid, "WindHeight" = windHeight, "ts" = ts,"units"=Units);
    }
    names(prcp )     <- paste('ts',as.character(1:NumMeteoTS),sep='')
    names(temp )     <- paste('ts',as.character(1:NumMeteoTS),sep='')
    names(rh   )     <- paste('ts',as.character(1:NumMeteoTS),sep='')
    names(winds)     <- paste('ts',as.character(1:NumMeteoTS),sep='')
    names(solar)     <- paste('ts',as.character(1:NumMeteoTS),sep='')
    names(longw)     <- paste('ts',as.character(1:NumMeteoTS),sep='')
    names(press)     <- paste('ts',as.character(1:NumMeteoTS),sep='')
   # names(data)<- paste('TS',as.character(1:NumMeteoTS),sep='')
    Units <- c("kg/m2/s","K","%","m/s","W/m2","W/m2","Pa")  
    prcp$unit	<-	Units[1];
    temp$unit	<-	Units[2];
    rh$unit	    <-	Units[3];
    winds$unit	<-	Units[4];
    solar$unit	<-	Units[5];
    longw$unit	<-	Units[6];
    press$unit	<-	Units[7];
   
    rawForcing<-list(
            'PRCP'  =	prcp    ,
            'SFCTMP'=	temp    ,
            'RH'    =	rh  ,
            'SFCSPD'=	winds   ,
            'SOLAR' =	solar   ,
            'LONGWV'=	longw   ,
            'PRES'  =	press ,
            'NumMeteoTS'=NumMeteoTS);
    
   # names(rawForcing[1:7]) <- c("PRCP","SFCTMP","RH","SFCSPD","SOLAR","LONGWV","PRES")
    #rawForcing$data<-data;
    #rawForcing$NumMeteoTS <- NumMeteoTS;
    #rawForcing$Unit <- c("kg/m2/s","K","%","m/s","W/m2","W/m2","Pa");
    return(rawForcing)
}


plotforc <- function(forc){
    if (missing(forc)){
        forc <- readforc();
    }
    
    names=names(forc);
    for(i in 1:length(names)){
        imgfile=file.path(ModelInfopath,paste(names[i],".png",sep=''))
        png(imgfile,width=1600, height=1600)
        q <- forc[[names[i]]]
        n =length(q)-1;
        qq <- q[[i]];
#        qq <- colMeans(q[1:n]);
        plot(qq,type='l')
        dev.off() 
    }

}
#==================================================
#       read forc for 2.0 version 
#==================================================
