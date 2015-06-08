

diffcalib<-function(dir,flist){
    if (missing(flist)){
        if(missing(dir)){
            dir='./';
             flist=list.files(dir)
             dirlist=list.files(dir,full.name=TRUE)
        }
    }else{
    }
    nf=length(flist);
    clist<-list();
    for(i in 1:nf){
        calib<-getcalibbak(dirlist[i])
        clist[[i]]=calib;
    }
    cmat=matrix(unlist(clist),ncol=nf)
    colnames(cmat)=flist;
    rownames(cmat)=names(calib);
    
    return(unique(cmat))
}

getcalibbak <- function(dir){
    
    theFile <- list.files(path=dir, pattern=paste(".",'calib.bak$',sep=''),full.names=TRUE);
    if (length(theFile)<=0){      
        stop ("\n\n\n .calib file \'", theFile , "\' is missing\n\n");
    }
    if (length(theFile)>1){
        stop ("\n\n\n Two .calib files", theFile );
    }

namelist <- character()
comments <- character();
value <- vector()
offon <- logical(length(namelist));
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

    names(value) <- toupper(namelist);
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
    return(value);
}

