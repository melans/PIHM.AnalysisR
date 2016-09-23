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
    
        rdsfile=file.path(inpath,paste(projectname,".meteo.RData",sep=""));

   if (file.exists(rdsfile)){
       ret=readRDS(file=rdsfile);  
   }else{
        if (pihmver >= 2.5){            
             ret <- readforcmm(p.only=FALSE);
        }else if (pihmver >=2.4) {
            ret <- readforcmf(p.only=FALSE);
        }else{
           ret <- readforc.22(p.only=FALSE);
 
        }
    }

    return(ret);
}

readforc.22 <- function(p.only=TRUE, start=as.POSIXct('2000-01-01','UTC')){ 
    theFile = getFilePath(ext='forc$', bak=FALSE)
    message('loading file: ', theFile);
    tlines <- readLines(theFile, skipNul=TRUE);    
    head=as.numeric(read.table(text=tlines[1]))
    nh = length(head)
    #tmp=scan(text = tlines[-1],what=character(), quiet=TRUE)
    nline = length(tlines)
    ord = 1:nline;
    lid = c( ord[grepl('[a-z]|[A-Z]', tlines) ], nline)
    nf = length(lid)-1;
    mat=list(nf)
    vnames = character(nf) 
    message('Start Time Defined as', strftime(start));
    for (i in 1:nf) {
            str = as.matrix(read.table(text = tlines[lid[i]] ))[1]
            tmp = unique( read.table(text = tlines[ (lid[i] + 1):(lid[i+1] -1) ] ))
            t = start + tmp[,1] * (as.POSIXct('2000-01-02','UTC') - as.POSIXct('2000-01-01','UTC'));
            ts = as.xts( tmp[,2], order.by = t)
            message(str);
        vnames[i]= str;
        mat[[i]] =  ts
    }
    sc = cumsum(head)
    prcp  = cbind(mat[[which(grepl('^pr', tolower(vnames)))]] )
    temp  = cbind(mat[[which(grepl('^temp', tolower(vnames)))]] )
    rh    = cbind(mat[[which(grepl('^rh', tolower(vnames)))]] )
    winds = cbind(mat[[which(grepl('^wind', tolower(vnames)))]] )
    solar = abind(mat[[which(grepl('^rn', tolower(vnames)))]] )
    LAI = as.xts( abind(mat[which(grepl('^lai', tolower(vnames)))] ) , order.by = time(mat[[sc[8]]]) )
    RL = as.xts ( abind(mat[which(grepl('^rl', tolower(vnames)))] ), order.by = time(mat[[sc[8]]]) )
    MF = abind(mat[which(grepl('^mf', tolower(vnames)))] )
    NumMeteoTS =  head[1] ;
    Forcing<-list(
                'PRCP'  =	prcp    ,
                'SFCTMP'=	temp    ,
                'RH'    =	rh  ,
                'SFCSPD'=	winds   ,
                'SOLAR' =	solar   ,
                'LAI'   =   LAI,
                'RL'    =   RL,
                'MF'    =   MF,
                'NumMeteoTS'=NumMeteoTS); 
    
    return(Forcing)
}

readforcmm <- function(p.only=TRUE){ 
    forcfile=list.files(inpath,pattern=paste(projectname,".meteo",sep=""),full.names=TRUE);
    if (!file.exists(forcfile)){
        stop("Error: file does not exist\n\t",forcfile, "\n");
    }
    tlines <- readLines(forcfile, skipNul=TRUE);    
    rids=which(unlist(lapply(tlines, FUN=function(x) grepl('^METEO_TS', x))))
    NumMeteoTS=length(rids)
    rids = c(rids, length(tlines))
    
    for (i in 1:NumMeteoTS){
        message('Parsing ',i,'/',NumMeteoTS,' sites\n');
        
        str <- unlist(strsplit(tlines[rids[i]],' +'));
        mid <- as.numeric(str[2]);  #metero id
        windHeight <- as.numeric(str[4])
        r0 = rids[i]+1   #start row
        r1 = rids[i+1]-1    #end row
        rid = c(r0, (r0+2):r1)
        
        #        tmplist <- scan(text = sub[!is.na(sub)] ,
#                        what=list('character','character', 'numeric','numeric','numeric','numeric','numeric','numeric','numeric'),
#                        quiet = TRUE)
        tmplist=scan(text=tlines[rid],skip=skip,n=nlines * 9 ,
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
        rdsfile=file.path(inpath,paste(projectname,".meteo.RData",sep=""));
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
#==================================================
#==========read forc for MF version ===============
#==================================================
readforcmf <- function(p.only=TRUE){ 
    forcfile=list.files(inpath,pattern=paste(projectname,".forc",sep=""),full.names=TRUE);
    if (!file.exists(forcfile)){
        stop("Error: file does not exist\n\t",forcfile, "\n");
    }
    tlines <- readLines(forcfile, skipNul=TRUE);    
    #NumMeteoTS=as.numeric(read.table(text=lines[1], row.names=1, header=FALSE))
    x=which(unlist(lapply(tlines, FUN=function(x) grepl('^METEO_TS', x))))
    
#    tlines = scan(file=forcfile,skip=0,nlines=1 ,blank.lines.skip = TRUE, what='character', quiet = TRUE)

    NumMeteoTS <- length(x);

    gret = system(paste('grep ^METEO_TS',forcfile,'-n ') , ignore.stdout = FALSE, intern=TRUE)
    rids= as.numeric(matrix(unlist(strsplit(gret,split=':')),nrow=2)[1,])
    rids = c(rids, length(tlines) );

    for (i in 1:NumMeteoTS){
        message('Parsing ',i,'/',NumMeteoTS,' sites\n');
        hd <-  read.table(text=tlines[rids[i]]);
        mid <- as.numeric(hd[2]);  #metero id
        windHeight <- as.numeric(hd[4])
        str <- tlines[(rids[i]+3): (rids[i+1]-1 )]
        nlines=rids[i+1]-rids[i]-3 
        
        if (i< NumMeteoTS && length(rids)>1 ){
            sub <- tlines[rids[i]+3:rids[i+1]-1]
            skip=rids[i]+2;
            nlines=rids[i+1]-rids[i]-3
        }else{
            sub <- tlines[rids[i]+3:length(tlines)]
            skip=rids[i]+2;
            nlines=length(tlines)-rids[i]
        } 
#        tmplist <- scan(text = sub[!is.na(sub)] ,
#                        what=list('character','character', 'numeric','numeric','numeric','numeric','numeric','numeric','numeric'),
#                        quiet = TRUE)
        tmplist = scan(text=str[1:1000],  what=list('character','character', 'numeric','numeric','numeric','numeric','numeric','numeric','numeric'))
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
        rdsfile=file.path(inpath,paste(projectname,".meteo.RData",sep=""));
        saveRDS(Forcing,file=rdsfile,compress=TRUE);
    }
        rdsfile=file.path(inpath,paste(projectname,".Prcp.RData",sep=""));
        saveRDS(prcp,file=rdsfile,compress=TRUE);
    if(p.only){
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
    forcrdsfile=file.path(inpath,paste(projectname,".meteo.RData",sep=""));
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
