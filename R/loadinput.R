#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#'  <- ============================================
#' @param  Path of output folder.
#' @keywords read output
#' @export  output data.
#' @examples
#' PIHM()

loadinput <- function(){
    mesh <- readmesh();
    riv <- readriv();
    soil <- readsoil();
    geol <- readgeol();
    att <- readatt();

    
    para <- readpara();
    calib <- readcalib();
    init <- readinit();
    
    #ibc <- readibc();
    ibc <-0;
    forc <- readforc();
pihmin <- list("ncell"=mesh$size[1],"nriv"=riv$River$size, "npt"=mesh$size[2],"mesh"=mesh, "soil"=soil, "geol"=geol, "att"=att, "forc"=forc, "riv"=riv, "para"=para, "calib"=calib, "init"=init, "ibc"=ibc);
return(pihmin)
}

#============ #============
#============ #============
readpara <- function(){
    theFile <- file.path(inpath, paste(projectname,".",'para',sep=''));

if (!file.exists(theFile)){
    stop ("\n\n\n file \'", theFile , "\' is missing\n\n");
}

para <- list();
namelist <- character()
comments <- character();

if (pihmver >2.3){
    lines <- readLines(theFile);   
    rid <- which(grepl('^start',tolower(lines)))
    str <- scan(text=lines[rid],what=character(),,quiet = TRUE);
    para[[1]]=as.POSIXct(paste(str[2],str[3]),format='%Y-%m-%d %H:%M',tz='UTC');
    namelist[1] <- 'START';

    rid <- which(grepl('^end',tolower(lines)))
    str <- scan(text=lines[rid],what=character(),,quiet = TRUE);
    para[[2]]=as.POSIXct(paste(str[2],str[3]),format='%Y-%m-%d %H:%M',tz='UTC');
    namelist[2]<- 'END';

    i=2; 
    for(k in 1:length(lines) ){
        
        str <- scan(text=lines[k],what=character(),quiet = TRUE);
        if (grepl("^[[:digit:]]",str[2]) && nchar(str[2])<10 && !grepl("^#$",str[1]) ){
#            cat (i ,'\t',str,'\n')
            i=i+1;
            namelist[i]=str[1];
            para[[i]]=as.numeric(str[2]);
            if (length(str)>2 ){
                comments[i] <- toString(str[-c(1,2)]) ;
            }else{
                comments[i] ='';
            }
        }
    }
    names(para) <- namelist;
    offon <- logical(length(namelist));
    for (i in 1:length(namelist)){
        if (grepl("^#",namelist[i])) {# if line start with '#', means off;
            offon[i]<-FALSE
        }
    }
    para$offon<-offon;
    para$comments <- comments;

}
else{
    #read .para file for pihm v2.0 - 2.2
}

    return(para);
}
#============ #============
#============ #============
readcalib <- function(){
    theFile <- file.path(inpath, paste(projectname,".",'calib',sep=''));

if (!file.exists(theFile)){
    stop ("\n\n\n .calib file \'", theFile , "\' is missing\n\n");
}

namelist <- character()
comments <- character();
value <- vector()
offon <- logical(length(namelist));
if (pihmver >2.3){
    lines <- readLines(theFile);   
    i=0; 
    for(k in 1:length(lines) ){
        str <- scan(text=lines[k],what=character(),quiet = TRUE);
        keyword <- str[1];
        if (grepl("^[[:digit:]]",str[2]) && nchar(str[2])<10 && !grepl("^#$",str[1]) ){
#            cat (i ,'\t',str,'\n')
            i=i+1;
            offon[i] <- TRUE
            while (grepl("^#",keyword) ){
                keyword <- substr(keyword,2,nchar(keyword))
                offon[i] <- FALSE
            }
            namelist[i]=keyword;
            value[i]=as.numeric(str[2]);
            names(value[i]) <- keyword
            if (length(str)>2 ){
                comments[i] <- toString(str[-c(1,2)]) ;
            }else{
                comments[i] ='';
            }
        }
    }

    names(value) <- namelist;


#    for (i in 1:length(namelist)){
#        if (grepl("^#",namelist[i])) {# if line start with '#', means off;
#            offon[i] <- FALSE
#        }else{
#            offon[i] <- TRUE
#        }
#    }
    onid <- which(offon);    
    dup <- which(duplicated(namelist[onid]) );    
    if (length(dup)>0){
        for (i in dup) {
            warning('Duplicated calibration parameter is set. You may get unexpected results. \n',
                    namelist[i],' = ',value[i],'\n');
        }
        undup <- which(!duplicated(namelist[onid]) );   
        value <- value[undup];
        offon <- offon[undup];
        names <- namelist[undup];
    }
    calib <- list('value'=value,
                  'offon'= offon, 
                  'comments' = comments);
}
else{
    #read .calib file for pihm v2.0 - 2.2
}
    return(calib);
}


#============ #============
#============ #============
readinit <- function(ncell,nriv){
     theFile <- file.path(inpath, paste(projectname,".",'init',sep=''));

if (!file.exists(theFile)){
    stop ("\n\n\n . init file \'", theFile , "\' is missing\n\n");
}
    
    mhead=c('IS','Snow','Overland','Unsat','Sat');
    rhead=c('RiverState','SatUndRiv');


    lines <- readLines(theFile);
    a<-lapply(lines,function(x) scan(text=x,what=numeric(),quiet = TRUE));
    lid <- which(lapply(a,length)==length(mhead));
#    minit<-do.call(rbind,a[lid])    # both lines work.
    minit <- t(matrix(unlist(a[lid]),nrow=length(mhead)) ); #,nrow=length(lid))
    rinit <- t(matrix(unlist(a[-lid]),nrow=length(rhead)) ); #nrow=length(a)-length(lid))    
    colnames(minit)= mhead
    colnames(rinit)= rhead;

    return(list('minit'=minit,'rinit'=rinit));
}
#============ #============
#============ #============
readibc <- function(){
    return(ibc);
}
#============ #============
#============ #============


#============ #============
#============ #============
readinTable <- function(ext, ncol, head){  #Basic function for read table for PIHM input. Applied for soil/geol//lc
theFile <- file.path(inpath, paste(projectname,".",ext,sep=''));

if (!file.exists(theFile)){
    stop ("\n\n\n file \'", theFile , "\' is missing\n\n");
}
if (pihmver >2.3){
    head=scan(theFile,what=character(),nlines=1,quiet = TRUE,blank.lines.skip = TRUE);
    head[1]='IND';
}else{
    head=head;
}

mat <-t( matrix (scan(theFile,what=numeric(),skip=1,blank.lines.skip = TRUE,quiet = TRUE), nrow=ncol))
colnames(mat)=head;
return(mat);
}

#============ #============
#============ #============
readsoil <-function(){
    
    head=c( "IND","INFK","MAXSMC","MINSMC","DINF","ALPHA","BETA","MACHF","SATMACHK","QTZ")
    mat <- readinTable('soil',length(head),head);
return(mat);
}

#============ #============
#============ #============
readgeol <-function(){
    head=c( "IND","SATHK","SATDK","MAXSMC","MINSMC","ALPHA","BETA","MACVF","SATMACKH","DMAC")
    mat <- readinTable('geol',length(head),head);
return(mat);
}

#============ #============
#============ #============
readlc <-function(fn){
}

projectInfo <- function (print=FALSE){
    if (missing('PIHMIN')){
        PIHMIN <- loadinput();
         assign("PIHMIN",pihmin , envir = .GlobalEnv)  
    }
    else{
        message('\ninpath =', inpath);
        message('\nprojectname = ', projectname);
        

    }
}
