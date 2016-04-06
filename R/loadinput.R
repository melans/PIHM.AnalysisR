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
    #forc <- readforc();
    
pihmin <- list("ncell"=mesh$size[1],"nriv"=riv$River$size, "npt"=mesh$size[2],"mesh"=mesh, "soil"=soil, "geol"=geol, "att"=att, "forc"=forc, "riv"=riv, "para"=para, "calib"=calib, "init"=init, "ibc"=ibc);
return(pihmin)
}

#============ #============
#============ #============
getoutlets <- function(if.river=T){
    riv=readriv();
    if(if.river){
        return(riv$River$outlets)
    }
    else{
       
        x=riv$River$outlets
        nx=length(x)
        pts=riv$River$riv[x,'TO']
        msh=readmesh(bak=FALSE)$mesh;
        mat=matrix(0, nx,5)
        for (i in 1:nx){
            noid = which(msh[,2]==pts[i] |msh[,3]==pts[i] |msh[,4]==pts[i])
            nbid = which(     msh[,5]<=0 | msh[,6]<=0 | msh[,7]<=0   )
            cellid = noid[which(noid %in% nbid)]
            mat[i,1]=i
            mat[i,2:3]=cellid
            mat[i,4] = which(msh[cellid[1], 5:7] <=0)
            mat[i,5] = which(msh[cellid[2], 5:7] <=0)           
        }
        colnames(mat)=c('ID','CellID1','CellID2','EdgeID1','EdgeID2')
        return(mat)
    }
}
readatt <-function(bak=FALSE, attfile=paste(projectname,".",'att',sep='')){
    if (bak){
        theFile <- list.files(path=outpath, pattern=paste(projectname,".",'att.bak',sep=''),full.names=TRUE);
        if (length(theFile)<=0){
            warning('The att file in input folder was read, instead of in output folder.\n');
            theFile <- list.files(path=inpath, pattern=attfile,full.names=TRUE);
        }        
            
    }else{
        theFile <- list.files(path=inpath, pattern=paste(attfile,'$',sep=''),full.names=TRUE);
    }

    if (length(theFile)<=0){
        stop ("\n\n\n file \'", theFile , "\' is missing\n\n");
    }
lines <- readLines(theFile);

if (pihmver >2.3){
    atthead=scan(text=lines[1],what=character(),nlines=1,quiet = TRUE,blank.lines.skip = TRUE);
}else{
    atthead=c( "IND",  "SOIL", "GEOL", "LC",   "CMC",  "SNOWH","HSFC", "UNSAT","GW",  "METEO","LAI",  "SS",   "BC0",  "BC1",  "BC2",  "MACP")
}
lines<-lines[!is.na(lines)];
matatt <- t(matrix(scan(text=lines[2:length(lines)],what=integer(),quiet = TRUE,blank.lines.skip = TRUE), nrow=16));
colnames(matatt)=toupper(atthead);
return(matatt);
}
#============ #============
#============ #============
readpara <- function(bak=FALSE){
    if (bak){
        theFile <- list.files(path=outpath, pattern=paste(projectname,".",'para.bak',sep=''),full.names=TRUE);
    }else{
        theFile <- list.files(path=inpath, pattern=paste(projectname,".",'para',sep=''),full.names=TRUE);
    }

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
    names(para) <- toupper(namelist);
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
readcalib <- function(bak, folder){
    if(missing(folder)){
        if (bak){
            theFile <- list.files(path=outpath, pattern=paste(projectname,".",'calib.bak$',sep=''),full.names=TRUE);
            if (length(theFile)<=0){
                cat('\n.calib file does not exist. Read the .calib in input folder instead? (YES/no)\n\n');
                line=readline()
                if ( grepl('^n', tolower(line)) ){
                    stop(' Abort. \n');
                }else{
                    warning('The calib file in input folder was read, instead of in output folder.\n');
                    theFile <- list.files(path=inpath, pattern=paste(projectname,".",'calib$',sep=''),full.names=TRUE);
                }
            }
        }else{
            theFile <- list.files(path=inpath, pattern=paste(projectname,".",'calib$',sep=''),full.names=TRUE);
        }
    }else{
        if(bak){
            theFile <- list.files(path=folder,
                                  pattern=paste(projectname,".",'calib.bak$',sep=''),full.names=TRUE);
        }else{
            theFile <- list.files(path=folder,
                                  pattern=paste(projectname,".",'calib$',sep=''),full.names=TRUE);

        }
    }
    
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
if (pihmver >2.3){
    lines <- readLines(theFile);   
    i=0; 
    for(k in 1:length(lines) ){
        str <- scan(text=lines[k],what=character(),quiet = TRUE);
        keyword <- str[1];
        if (grepl("^[[:digit:]]",str[2]) && nchar(str[2])<20 && !grepl("^#$",str[1]) ){
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
readinit <- function(bak=FALSE,update=FALSE){
     theFile <- file.path(inpath, paste(projectname,".",'init',sep=''));
    if(bak){
        theFile <- file.path(outpath, paste(projectname,".",'init.bak',sep=''));
    }
    if(update){
        theFile <- file.path(outpath, paste(projectname,".",'init.update',sep=''));
    }

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
    colnames(minit)= toupper(mhead)
    colnames(rinit)= toupper(rhead);

    return(list('minit'=minit,'rinit'=rinit));
}
#============ #============
#============ #============
readibc <- function(bak=FALSE){
    return(ibc);
}
#============ #============
#============ #============


#============ #============
#============ #============
readinTable <- function(ext, ncol, head,bak=FALSE){  #Basic function for read table for PIHM input. Applied for soil/geol//lc
   if (bak){
        theFile <- list.files(path=outpath, pattern=paste(projectname,".",ext,'.bak',sep=''),full.names=TRUE);
            if (length(theFile)<=0){
                warning('The ',ext,' file in input folder was read, instead of in output folder.\n');
                theFile <- list.files(path=inpath, pattern=paste(projectname,".",ext,'$',sep=''),full.names=TRUE);
            }
        
    }else{
        theFile <- list.files(path=inpath, pattern=paste(projectname,".",ext,sep=''),full.names=TRUE);
    }

    if (!file.exists(theFile)){
        stop ("\n\n\n file \'", theFile , "\' is missing\n\n");
    }

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
colnames(mat)=toupper(head);
return(mat);
}

#============ #============
#============ #============
readsoil <-function(bak=FALSE){
    
    head=c( "IND","INFK","MAXSMC","MINSMC","DINF","ALPHA","BETA","MACHF","SATMACVK","QTZ")
    mat <- readinTable('soil',length(head),head,bak=bak);
return(mat);
}

#============ #============
#============ #============
readgeol <-function(bak=FALSE){
    head=c( "IND","SATHK","SATVK","MAXSMC","MINSMC","ALPHA","BETA","MACVF","SATMACKH","DMAC")
    mat <- readinTable('geol',length(head),head,bak=bak);
return(mat);
}

#============ #============
#============ #============
readlc <-function(bak=TRUE,path=outpath){
    if (bak){
        theFile <- list.files(path=path, pattern=paste(projectname,".",'lc','.bak',sep=''),full.names=TRUE);
    }else{
        theFile <- list.files(path=path, pattern=paste(projectname,".",'lc$',sep=''),full.names=TRUE);
    }

    if (!file.exists(theFile)){
        stop ("\n\n\n file \'", theFile , "\' is missing\n\n");
    }

    tmp=read.table(theFile);
    lc=as.matrix(tmp);
    if(lc[1,1]==0){
        lc=lc[-1,]
    }
    colnames(lc)=c("INDEX","SHDFAC","DROOT","RS","RGL",
                   "HS","SNUP","LAIMIN","LAIMAX",
                   "EMISMIN","EMISMAX","ALBMIN","ALBMAX",
                   "Z0MIN","Z0MAX","ROUGH","ImpArea"
                   );
    #head=c();
    return(lc);
}

#============ #============
#============ #============
showmeKs <- function( calib.bak=FALSE,quiet=FALSE,path=Resultpath){
    soil<-readsoil();
    geol<-readgeol();
    calib<-readcalib(bak=calib.bak);
    ns=nrow(soil);
    ng=nrow(geol);
    if(ns>1 & ng>1){
        id= which(grepl('infk|satmac',tolower(colnames(soil))) )
        Ksoil<-soil[,id]
        if(ns>1){
            Ksoil<-rbind(Ksoil,colMeans(Ksoil));
            row.names(Ksoil) <- c(paste(1:ns),'mean');
        }

        geolcols= which(grepl('^sat|^satmac',tolower(colnames(geol))) )

        Kgeol<-geol[,geolcols];
        if(ng>1){
            Kgeol<-rbind(Kgeol,colMeans(Kgeol));
            row.names(Kgeol) <- c(paste(1:ng),'mean');
        }

        cKsoil<-Ksoil;
        cKgeol<-Kgeol;
        
        cKsoil[,'INFK'] = cKsoil[,'INFK']*calib$value['KINF'];
        
        id=2
        cKsoil[,id] = cKsoil[,id]*calib$value['KMACSATV'];

        id=1 #which(grepl('^sathk',tolower(colnames(geol))) )
        cKgeol[,id] = cKgeol[,id]*calib$value['KSATH'];

        id=2 #which(grepl('^satvk|^satdk',tolower(colnames(geol))) )
        cKgeol[,id] = cKgeol[,id]*calib$value['KSATV'];
        id=3 #which(grepl('^satmac',tolower(colnames(geol))) );
        cKgeol[,id] = cKgeol[,id]*calib$value['KMACSATH'];

        riv=readriv(bak=TRUE);
        nr=nrow(riv$Material$mat);
        if (nr>1){
            Krivm=riv$Material$mat[,4:5];
            
            id=which(grepl('^krivh',tolower(names(calib$value))) )
            Krivm=cbind(Krivm,Krivm[,1]*calib$value[id]);
            id=which(grepl('^krivv',tolower(names(calib$value))) )
            Krivm=cbind(Krivm,Krivm[,2]*calib$value[id]);
            colnames(Krivm)=c(colnames(Krivm[,1:2]), paste('C_',colnames(Krivm[,1:2]),sep=''));
            if(nrow(Krivm)>1){
                Krivm=rbind(Krivm,colMeans(Krivm));
                row.names(Krivm)=c(paste(1:nr),'mean');
            }
        }else{
            Krivm=riv$Material$mat[,4:5];
            
            id=which(grepl('^krivh',tolower(names(calib$value))) )
            Krivm=c(Krivm,Krivm[1]*calib$value[id]);
            id=which(grepl('^krivv',tolower(names(calib$value))) )
            Krivm=c(Krivm,Krivm[2]*calib$value[id]);
            names(Krivm)=c(names(Krivm[1:2]), paste('C_',names(Krivm[1:2]),sep=''));
           # Krivm=t(as.matrix(Krivm,nrow=1))
        }
        
        if( !quiet){ 
            cat('\nK in soil/geol with calib, (m/s)\n');
            print(Ksoil[nrow(Ksoil),] )
            print(Kgeol[nrow(Kgeol),] )
            cat('\nK in soil/geol with calib, (m/s)\n');
            print(cKsoil[nrow(cKsoil),] )
            print(cKgeol[nrow(cKgeol),] )
            cat('\nK in riv, (m/s)\n');
            print(Krivm);
        #    print("K in soil\t=\t", last(Ksoil),' m/s\n');
        #    print("K in geol\t=\t", last(Kgeol),' m/s\n');
        #    print("K in calib+soil\t=\t", last(cKsoil),' m/s\n');
        #    print("K in calib+cKgeol\t=\t", last(cKgeol),' m/s\n');

        }

        if (calib.bak){
            theFile <- file.path(path, paste(projectname,".",'K_SoilGeolRiv.txt',sep=''));
            write('K in soil/Geol (m/s)\n\n',theFile, append=FALSE);
            write.table(Ksoil[nrow(Ksoil) ,],quote=FALSE,theFile,append=TRUE,row.names=TRUE,col.names=TRUE,eol = "\n");
            write.table(Kgeol[nrow(Kgeol),],quote=FALSE,theFile,append=TRUE,row.names=TRUE,col.names=TRUE,eol = "\n");
            
            write.table('\n=====With calib=====',quote=FALSE,theFile,append=TRUE,row.names=FALSE,col.names=FALSE,eol = "\n");
            
            write.table(cKsoil[nrow(cKsoil),],quote=FALSE,theFile,append=TRUE,row.names=TRUE,col.names=TRUE,eol = "\n");
            write.table(cKgeol[nrow(cKgeol),],quote=FALSE,theFile,append=TRUE,row.names=TRUE,col.names=TRUE,eol = "\n");
            write.table(Krivm,theFile,append=TRUE,row.names=TRUE,col.names=TRUE,,quote=FALSE,eol = "\n");

    #         lapply(Ksoil, write.table, theFile, append=TRUE)
    #         lapply(Kgeol, write.table, theFile, append=TRUE)
    #         lapply(cKsoil, write.table, theFile, append=TRUE)
    #         lapply(cKgeol, write.table, theFile, append=TRUE)
        }
    }else{
        Ksoil=soil[,c(2,9)];
        Kgeol=geol[,c(2,3,9)];
        cKsoil=Ksoil;
        cKgeol=Kgeol;
        cKsoil['INFK'] = cKsoil['INFK']*calib$value['KINF'];
        id=2
        cKsoil[id] = cKsoil[id]*calib$value['KMACSATV'];

        id=1 #which(grepl('^sathk',tolower(colnames(geol))) )
        cKgeol[id] = cKgeol[id]*calib$value['KSATH'];

        id=2 #which(grepl('^satvk|^satdk',tolower(colnames(geol))) )
        cKgeol[id] = cKgeol[id]*calib$value['KSATV'];
        id=3 #which(grepl('^satmac',tolower(colnames(geol))) );
        cKgeol[id] = cKgeol[id]*calib$value['KMACSATH'];
        
        riv=readriv(bak=TRUE);
        nr=nrow(riv$Material$mat);
        if (nr>1){
            Krivm=riv$Material$mat[,4:5];
            
            id=which(grepl('^krivh',tolower(names(calib$value))) )
            Krivm=cbind(Krivm,Krivm[,1]*calib$value[id]);
            id=which(grepl('^krivv',tolower(names(calib$value))) )
            Krivm=cbind(Krivm,Krivm[,2]*calib$value[id]);
            colnames(Krivm)=c(colnames(Krivm[,1:2]), paste('C_',colnames(Krivm[,1:2]),sep=''));
            if(nrow(Krivm)>1){
                Krivm=rbind(Krivm,colMeans(Krivm));
                row.names(Krivm)=c(paste(1:nr),'mean');
            }
        }else{
            Krivm=riv$Material$mat[,4:5];
            
            id=which(grepl('^krivh',tolower(names(calib$value))) )
            Krivm=c(Krivm,Krivm[1]*calib$value[id]);
            id=which(grepl('^krivv',tolower(names(calib$value))) )
            Krivm=c(Krivm,Krivm[2]*calib$value[id]);
            names(Krivm)=c(names(Krivm[1:2]), paste('C_',names(Krivm[1:2]),sep=''));
           # Krivm=t(as.matrix(Krivm,nrow=1))
        }
 
        if( !quiet){ 
            cat('\nK in soil/geol with calib, (m/s)\n');
            print(Ksoil)
            print(Kgeol)
            cat('\nK in soil/geol with calib, (m/s)\n');
            print(cKsoil)
            print(cKgeol)
            cat('\nK in riv, (m/s)\n');
            print(Krivm);
        #    print("K in soil\t=\t", last(Ksoil),' m/s\n');
        #    print("K in geol\t=\t", last(Kgeol),' m/s\n');
        #    print("K in calib+soil\t=\t", last(cKsoil),' m/s\n');
        #    print("K in calib+cKgeol\t=\t", last(cKgeol),' m/s\n');

        }

        if (calib.bak){
            theFile <- file.path(path, paste(projectname,".",'K_SoilGeolRiv.txt',sep=''));
            write('K in soil/Geol (m/s)\n\n',theFile, append=FALSE);
            write.table(Ksoil,quote=FALSE,theFile,append=TRUE,row.names=TRUE,col.names=TRUE,eol = "\n");
            write.table(Kgeol,quote=FALSE,theFile,append=TRUE,row.names=TRUE,col.names=TRUE,eol = "\n");
           
            write.table('\n=====With calib=====',quote=FALSE,theFile,append=TRUE,row.names=FALSE,col.names=FALSE,eol = "\n");
            
            write.table(cKsoil,quote=FALSE,theFile,append=TRUE,row.names=TRUE,col.names=TRUE,eol = "\n");
            write.table(cKgeol,quote=FALSE,theFile,append=TRUE,row.names=TRUE,col.names=TRUE,eol = "\n");
            write.table(Krivm,theFile,append=TRUE,row.names=TRUE,col.names=TRUE,,quote=FALSE,eol = "\n");

    #         lapply(Ksoil, write.table, theFile, append=TRUE)
    #         lapply(Kgeol, write.table, theFile, append=TRUE)
    #         lapply(cKsoil, write.table, theFile, append=TRUE)
    #         lapply(cKgeol, write.table, theFile, append=TRUE)
        }

    }
    Ks<-list('Ksoil'=Ksoil,'Kgeol'= Kgeol,'cKsoil'=cKsoil,'cKgeol'=cKgeol,'Krivm'=Krivm, 'Unit'='m/s');


    return(Ks);
}
#======                      ================================
#======  ====   =   =  =   =  ===============================
#======  =      =   =  ==  =  ===============================
#======  ====   =   =  = = =  ===============================
#======  =      =   =  =  ==  ===============================
#======  =       ===   =   =  ===============================
#======                      ================================
Kcalib <-function(calib.bak=TRUE,quiet=FALSE,path=Resultpath){

K<-showmeKs(calib.bak=calib.bak,quiet=quiet,path=path); 

Kd <- lapply(K[!(names(K) %in% c('Unit'))],function(x) x*86400)
if (calib.bak){
    theFile <- file.path(path, paste(projectname,".",'K_SoilGeolRiv.txt',sep=''));
    write('\n\nK in soil/Geol (m/day)\n\n',theFile, append=TRUE);
    for(i in 1:2 ){
        write.table(last(Kd[[i]]),theFile,append=TRUE,row.names=TRUE,col.names=TRUE,,quote=FALSE,eol = "\n");
    }    
    write.table('\n=====With calib=====',quote=FALSE,theFile,append=TRUE,row.names=FALSE,col.names=FALSE,eol = "\n");
    for(i in 3:4 ){
        write.table(last(Kd[[i]]),theFile,append=TRUE,row.names=TRUE,col.names=TRUE,,quote=FALSE,eol = "\n");
    }    
    write.table(Kd[[5]],theFile,append=TRUE,row.names=TRUE,col.names=TRUE,,quote=FALSE,eol = "\n");

}
Kd <- c(Kd,'Unit'='m/day');

if(!quiet){
    cat('\n\nK in soil/geol without calib, (m/day)\n');
    for (i in 1:2){
        print( Kd[[i]][nrow(Kd[[i]]),]     );
    }
    cat('\nK in soil/geol with calib, (m/day)\n');
    for (i in 3:4){
        print( Kd[[i]][nrow(Kd[[i]]),]     );
    }
    cat('\nK in riv , (m/day)\n');
    print(  Kd[[5]]    );
    showcalib()
   # print(names(Kd));
}


return(Kd);

}

#============ #============
#============ #============
showcalib <- function(bak=TRUE){
    calib=readcalib(bak=bak)
    print(calib$value)
}


#============ #============
#============ #============
readmesh <-function(bak=FALSE, file){
    if(missing(file)){
        if(!bak){
            meshfile <- file.path(inpath, paste(projectname,".mesh",sep=''));
        }else{
            meshfile <- file.path(outpath, paste(projectname,".mesh.bak",sep=''));
            if(!file.exists(meshfile)){
                warning('No .mesh file exists in ',outpath,'\n');
                meshfile <- file.path(inpath, paste(projectname,".mesh",sep=''));
                #stop('No .mesh exists in folder ', outpath ,'\n');
            }
        }
    }else{
        meshfile =file
    }
    if (pihmver >=2.4 ){
        ncell=scan(meshfile,what=integer(),nmax=1,blank.lines.skip = TRUE,quiet = TRUE);
        mshhead=scan(meshfile,what=character(),nlines=1,blank.lines.skip = TRUE,quiet = TRUE);

        msh <-t( matrix (scan(meshfile,what=integer(),skip=1,nlines=ncell,blank.lines.skip = TRUE,quiet = TRUE), ncol=ncell))

        npt=scan(meshfile,what=integer(),nmax=1,skip=ncell+1,blank.lines.skip = TRUE,quiet = TRUE);
        pthead=scan(meshfile,what=character(),nlines=1,skip=ncell+1,blank.lines.skip = TRUE,quiet = TRUE);
        pt <-t( matrix (scan(meshfile,what=double(),skip=ncell+2,nlines=npt,blank.lines.skip = TRUE,quiet = TRUE), ncol=npt))

        colnames(msh)= c("ID",mshhead[-1]) 
        colnames(pt)=c("ID",pthead[-1]);
    }
    else{
        num=scan(meshfile,what=integer(),nmax=2,blank.lines.skip = TRUE,quiet = TRUE);
        ncell=num[1];
        npt=num[2];
        msh <-t( matrix (scan(meshfile,what=integer(),skip=1,nlines=ncell,blank.lines.skip = TRUE,quiet = TRUE), ncol=ncell))
        pt <-t( matrix (scan(meshfile,what=double(),skip=ncell+1,nlines=npt,blank.lines.skip = TRUE,quiet = TRUE), ncol=npt))

        colnames(msh) <- c("ID","NODE1","NODE2","NODE3","NABR1","NABR2","NABR3")
        colnames(pt) <- c("ID", "X","Y","ZMIN","ZMAX");

    }
    mesh <-list("size"=c(ncell,npt),"mesh"=msh, "points"=pt );
    return(mesh);
}

#============ #============
#============ #============
readarea <-function(bak=FALSE){
    mesh <- readmesh(bak=bak);
    msh <- mesh$mesh;
    pts <- mesh$points;
    tri <- msh[,2:4];
    m <- nrow(tri);
    x=t(matrix(c(pts[tri[,1],2],pts[tri[,2],2],pts[tri[,3],2],pts[tri[,1],2]),m,4) );
    y=t(matrix(c(pts[tri[,1],3],pts[tri[,2],3],pts[tri[,3],3],pts[tri[,1],3]),m,4) );
    z=t(matrix(c(pts[tri[,1],4],pts[tri[,2],4],pts[tri[,3],4],pts[tri[,1],4]),m,4) );
  iarea <- polyarea(x,y); 
  return(iarea);
  
}


#============ #============
#============ #============

getporosity <- function(bak=TRUE,if.calib=TRUE){
    
    att=readatt(bak=bak);
    soilid=att[,2];
    geolid=att[,3];
    
    soil=readsoil();
    geol=readgeol();
    
    sp=soil[soilid,3];
    gp=geol[geolid,4];

    poro=matrix(c(sp,gp),nrow=2);
    rownames(poro)=c('Soil_Poro','Geol_Poro');

    return(poro);
}
