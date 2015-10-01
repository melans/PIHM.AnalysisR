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
#    stop('Not ready\n');
    
        rdsfile=file.path(inpath,paste(projectname,".forc.RData",sep=""));

   if (file.exists(rdsfile)){
       forc=readRDS(file=rdsfile);  
       ret=forc;
   }else{
        if (pihmver > 2.3){
            forcing <- readforcmf(p.only=FALSE);
            ret=forcing;    
        }else{
            
        }
    }
    

    return(ret);
}

#==================================================
#==========read forc for MF version ===============
#==================================================
readforcmf <- function(p.only=TRUE){ 
    forcfile=list.files(inpath,pattern=paste(projectname,".forc",sep=""),full.names=TRUE);
    if (!file.exists(forcfile)){
        stop("Error: file does not exist\n\t",forcfile, "\n");
    }
  #  lines <- readLines(forcfile);
    lines = scan(file=forcfile,skip=0,nlines=1 ,blank.lines.skip = TRUE,
                     what='character',
                     quiet = TRUE)

    NumMeteoTS <- as.numeric(lines[2]);

    gret = system(paste('grep ^METEO_TS',forcfile,'-n ') , ignore.stdout = FALSE, intern=TRUE)
    rids= as.numeric(matrix(unlist(strsplit(gret,split=':')),nrow=2)[1,])
    
    for (i in 1:NumMeteoTS){
        message('Parsing ',i,'/',NumMeteoTS,' sites\n');
        str <- unlist(strsplit(lines[rids[i]],' +'));
        mid <- as.numeric(str[2]);  #metero id
        windHeight <- as.numeric(str[4])
        if (i< NumMeteoTS && length(rids)>1 ){
            sub <- lines[rids[i]+3:rids[i+1]-1]
            skip=rids[i]+2;
            nlines=rids[i+1]-rids[i]-3
        }else{
            sub <- lines[rids[i]+3:length(lines)]
            skip=rids[i]+2;
            nlines=length(lines)-rids[i]
        } 
#        tmplist <- scan(text = sub[!is.na(sub)] ,
#                        what=list('character','character', 'numeric','numeric','numeric','numeric','numeric','numeric','numeric'),
#                        quiet = TRUE)
        tmplist=scan(file=forcfile,skip=skip,n=nlines * 9 ,
                     what=list('character','character', 'numeric','numeric','numeric','numeric','numeric','numeric','numeric'),
                     quiet = TRUE)
        t <-
            as.POSIXct(paste(unlist(tmplist[[1]]),unlist(tmplist[[2]])),format='%Y-%m-%d %H:%M',tz='UTC');
#        matdata <- matrix(as.numeric(unlist(tmplist[3:length(tmplist)])),ncol=7);
       # prcp[[i]]   <- xts(as.numeric(unlist(tmplist[3])),order.by = t);
        if (i==1){
            prcp <- xts(as.numeric(unlist(tmplist[3])),order.by = t);
        }else{
            prcp <-cbind( prcp, xts(as.numeric(unlist(tmplist[3])),order.by = t) )
        }
        if(!p.only){
            if (i==1){
                temp   <- xts(as.numeric(unlist(tmplist[4])),order.by = t);
                rh     <- xts(as.numeric(unlist(tmplist[5])),order.by = t);
                winds  <- xts(as.numeric(unlist(tmplist[6])),order.by = t);
                solar  <- xts(as.numeric(unlist(tmplist[7])),order.by = t);
                longw  <- xts(as.numeric(unlist(tmplist[8])),order.by = t);
                press  <- xts(as.numeric(unlist(tmplist[9])),order.by = t);
            }else{
                temp   <- cbind( temp , xts(as.numeric(unlist(tmplist[4])),order.by = t))
                rh     <- cbind( rh   , xts(as.numeric(unlist(tmplist[5])),order.by = t))
                winds  <- cbind( winds, xts(as.numeric(unlist(tmplist[6])),order.by = t))
                solar  <- cbind( solar, xts(as.numeric(unlist(tmplist[7])),order.by = t))
                longw  <- cbind( longw, xts(as.numeric(unlist(tmplist[8])),order.by = t))
                press  <- cbind( press, xts(as.numeric(unlist(tmplist[9])),order.by = t))
            }
        }
    }
    
    if(!p.only){
        Forcing<-list(
                'PRCP'  =	prcp    ,
                'SFCTMP'=	temp    ,
                'RH'    =	rh  ,
                'SFCSPD'=	winds   ,
                'SOLAR' =	solar   ,
                'LONGWV'=	longw   ,
                'PRES'  =	press ,
                'NumMeteoTS'=NumMeteoTS);
        rdsfile=file.path(inpath,paste(projectname,".forc.RData",sep=""));
        saveRDS(Forcing,file=rdsfile,compress=TRUE);
    }
        rdsfile=file.path(inpath,paste(projectname,".Prcp.RData",sep=""));
        saveRDS(prcp,file=rdsfile,compress=TRUE);
    if(p.only){
        colnames(prcp)=NULL;
        prcp=as.matrix(prcp, ncol=NumMeteoTS);
        return(prcp)
    }else{
        for( i in 1: (length(Forcing)-1) ){
            Forcing[[i]]=as.matrix(Forcing[[i]], ncol=Forcing[[i]]);
            colnames(Forcing[[i]])=NULL;
        }

        return(Forcing);
    }
}


plotforc <- function(forc){
    stop('not yet');
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


readprcp <- function(period='daily'){
    forcrdsfile=file.path(inpath,paste(projectname,".forc.RData",sep=""));
    rdsfile=file.path(inpath,paste(projectname,".Prcp.RData",sep=""));
    if (file.exists(rdsfile)){
        prcp=readRDS(file=rdsfile); 
    }else if ( file.exists(forcrdsfile) ) {
        forc=readRDS(file=forcrdsfile) 
        prcp=forc$PRCP
    }else{
        prcp=readforcmf(p.only=TRUE);
    }
    colnames(prcp)=NULL;
    
    return(prcp)
}
dailyprcp <-function(prcp=readprcp(),ufactor=86400){
            P= prcp * ufactor;
            time(P)=round(time(P),units='days');
            ipd=apply.daily(P,FUN=mean);  
            dates=as.Date(time(ipd));
            att=readatt(bak=TRUE);
            mid <- att[,which(grepl('^meteo',tolower(colnames(att))))];
            iarea=readarea()
            area=sum(iarea)
    pdaily=rowSums(matRowMulti(ipd[,mid],iarea)) / area;
    pdaily=xts(pdaily, order.by=dates)
    
    return(pdaily);

}


#==================================================
#       read forc for 2.0 version 
#==================================================
