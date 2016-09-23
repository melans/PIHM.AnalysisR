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

loadinput <- function(reload=FALSE,bak=TRUE, resolution = 0, path = ModelInfopath){
    rdsfile =file.path(path, paste0(projectname, '.input.RDS'))
    if(! file.exists(rdsfile)){
        message('Reload the PIHM input data')
        reload=TRUE
    }
    
    if( reload ){
    mesh <- readmesh(bak = bak, shp=TRUE );
    soil <- readsoil(bak = bak );
    geol <- readgeol(bak = bak );
    att <- readatt(bak = bak );
    
    para <- readpara(bak = bak );
    calib <- readcalib(bak = bak );
    init <- readinit(bak = bak );

    meshatt = meshAtt(mesh=mesh, att=att, geol=geol, soil=soil, calib=calib)
    #ibc <- readibc();
    ibc <-0;
    #forc <- readforc();
   Shp.mesh = mesh$shp;
   ext = extent(Shp.mesh)
   if( resolution <=0 ){
      resolution = round(max(diff(ext[1:2]), diff(ext[3:4]))/200, -1)
   }
  message('Raster resolution is ', resolution);
  r=raster(ext=ext, res = resolution)
  r.elev = rasterize(Shp.mesh, r, field='Zmax')
  r.mask = abs(r.elev )+1
  r.mask = r.mask/r.mask;
  writeRaster(overwrite=TRUE, filename = file.path(ModelInfopath,'GISlayer','mask.mesh.asc'), r.mask)
  writeRaster(overwrite=TRUE, filename = file.path(ModelInfopath,'GISlayer','elev.asc'), r.elev)
  
   
pihmin <- list("ncell"=mesh$size[1],
            #   "nriv"=riv$River$size, 
               "npt"=mesh$size[2],
               "mesh"=mesh, 
               "soil"=soil,
               "geol"=geol, 
               "att"=att,
            #   "forc"=forc, 
            #   "riv"=riv, 
               "para"=para,
               "calib"=calib,
               "init"=init,
               "ibc"=ibc, 
               'Shp.mesh'= Shp.mesh,
           #    'Shp.riv'=rivshp,
               'r.elev'=r.elev,
               'r.mask'=r.mask,
               'meshatt'=meshatt
               );


  if(RIVERON){
      #River
    riv <- readriv(bak = bak );
      message('River shapefile...')
      rivshp = Riv2Shp(riv=riv, mesh=mesh, path=path)
      pihmin <- c(pihmin, 
                  list(
               "nriv"=riv$River$size, 
               "riv"=riv, 
               'Shp.riv'=rivshp)
                )                  
  } 
  if(LAKEON){
      message('Reading lake information...')
      lake.att=lake.readatt(bak=bak)
      lake.bathy=lake.readbathy(bak=bak)
      lake.geo = lake.readgeom(bak=bak);
      lake.shp = lake.boundnode()
      message('Generating lake raster...')
      lake.elev = rasterize(lake.shp, r.mask, field='bathy.SURF')
      make.mask = abs(lake.elev) +1;
      lake.mask = make.mask/make.mask
    fun=function(x,y){
        return(ifelse(is.na(x) & is.na(y), NA, 
                      ifelse(is.na(x), y,
                             ifelse(is.na(y),x, x+y)) )
        )
    }
    
    r.elev.all = overlay(lake.elev, r.elev, fun=fun)
    r.mask.all = overlay(lake.mask, r.mask, fun=fun)
  writeRaster(overwrite=TRUE, filename = file.path(ModelInfopath,'GISlayer','mask.lake.asc'), lake.mask)
  writeRaster(overwrite=TRUE, filename = file.path(ModelInfopath,'GISlayer','mask.all.asc'), r.mask.all)
  writeRaster(overwrite=TRUE, filename = file.path(ModelInfopath,'GISlayer','elev.lake.asc'), lake.elev)
  writeRaster(overwrite=TRUE, filename = file.path(ModelInfopath,'GISlayer','elev.allasc'), r.elev.all)
      
      pihmin <- c(pihmin, 
                  list(
                  "lake.att"=lake.att,
                  'lake.bathy' = lake.bathy,
                  'lake.geo' =lake.geo,
                  'Shp.lake'=lake.shp,
                  'r.elev.lake' = lake.elev,
                  'r.mask.lake' = lake.mask,
                  'r.mask.all'=r.mask.all,
                  'r.elev.all'=r.elev.all
                  )
                  )

  }
    
    message('Writing RDS of input files...')
saveRDS(file=rdsfile, pihmin)
    }
    else{
        pihmin=readRDS(file=file.path(path, paste0(projectname, '.input.RDS')))
    }
  assign("PIHMIN",pihmin , envir = .GlobalEnv)
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
    theFile = getFilePath('att');

lines <- readLines(theFile, skipNul=TRUE);

if (pihmver >2.3){
    atthead=scan(text=lines[1],what=character(),nlines=1,quiet = TRUE,blank.lines.skip = TRUE);
    matatt <- t(matrix(scan(text=lines[2:length(lines)],what=integer(),quiet = TRUE,blank.lines.skip = TRUE), nrow=length(atthead) ));
}else{
    atthead=c( "IND",  "SOIL", "GEOL", "LC",  
              "CMC",  "SNOWH","HSFC", "UNSAT","GW",  
              "PRCP", 'TEMP', 'HUM','WIND', 'RN', 'G', 'PRESS',
              'SS','MF',  "BC0",  "BC1",  "BC2",  "MACP")
    matatt <- as.matrix(read.table(text = lines) )
    #t(matrix(scan(text=lines[1:length(lines)],what=integer(),quiet = TRUE,blank.lines.skip = TRUE), nrow=length(atthead) ));
}

colnames(matatt)=toupper(atthead);
return(matatt);
}
#============ #============
#============ #============
readpara22 <- function(){
    theFile= getFilePath(ext='para', bak=FALSE);
    tlines = readLines(theFile, skipNul=TRUE)
    value = read.table(text= tline, col.names=1, header=FALSE)
    
    value = scan(theFile,what=numeric(), quiet=TRUE)
    
names(value)= c('VERBOSE', 'DEBUG', 'INIT_MODE', 
        'GW', 'SURF', 'SNOW', 'RIVSTG', 
        'RECHARGE', 'CMC', 'UNSAT', 
        'EC ', 'ETT', 'EDIR', 
        'RIVFLX0', 'RIVFLX1', 'RIVFLX2', 'RIVFLX3', 'RIVFLX4', 'RIVFLX5', 'RIVFLX6', 'RIVFLX7', 'RIVFLX8', 'RIVFLX9', 
        'D_GW', 'D_SURF', 'D_SNOW', 'D_RIVSTG', 
        'D_RECHARGE', 'D_CMC', 'D_UNSAT', 
        'D_ET', 'D_RIVFLX', 
        'UNSAT_MODE', 'SAT_MODE', 'RIV_MODE', 
        'SOLVER', 'GSTYPE', 'MAXK', 'DELTA', 
        'ABSTOL', 'RELTOL', 'INIT_SOLVER_STEP', 'MAX_SOLVER_STEP', 'LSM_STEP', 
        'START', 'END', 'OUTPUT_TYPE', 
        'STEPSIZE_FACTOR', 'MODEL_STEPSIZE')
    offon= NA
    comments = NA;
    para <- list('value'=value,
                  'offon'= offon, 
                  'comments' = comments);
    return(para)
}
readpara <- function(bak=FALSE){
    if(LAKEON){
        theFile = getFilePath(ext='para',  bak=bak)
        tline = readLines(theFile, skipNul = TRUE)
        tmp = which(grepl('[:Alpha:]', tline) | grepl('[:alpha:]', tline))
        if (length(tmp)>0 ){
            value = as.data.frame( read.table(text = tline, header=FALSE, row.names= 1) ) 
            x = unlist(value)
            names(x) = toupper(rownames(value))
        }   
        return(x);
    }
    if (pihmver<=2.2){
        x=readpara22();
        return(x)
    }

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

readcalib22 <- function(text){   
    value = scan(text=text,what=double(), quiet=TRUE)
names(value)= c('KSATH', 'KSATV', 'KINF', 'KMACSATH', 'KMACSATV', 'DINF', 'DROOT', 'DMAC',
        'POROSITY', 'ALPHA', 'BETA', 'MACVF', 'MACHF', 'VEGFRAC', 'ALBEDO', 'ROUGH',
        'PRCP', 'SFCTMP', 'EC', 'ETT', 'EDIR', 'ROUGH_RI', 'KRIVH', 'KRIVV', 'BEDTHCK',
        'RIV_DPTH', 'RIV_WDTH')
    return(value)
}
readcalib <- function(bak=TRUE, path=outpath){
    theFile = getFilePath(ext='calib', path=path, bak=bak)
    tline = readLines(theFile, skipNul = TRUE)
    tmp = which(grepl('[:Alpha:]', tline) | grepl('[:alpha:]', tline))
    if (length(tmp)>0 ){
        value = as.data.frame( read.table(text = tline, header=FALSE, row.names= 1) ) 
        calib = unlist(value)
        names(calib) = toupper(rownames(value))
    }else{
        calib = readcalib22(text = tline);       
    }
    return(calib);
}


#============ #============
#============ #============
readinit <- function(bak=FALSE,update=FALSE){
    if(update){
        ext='init.update'
        theFile <- getFilePath( bak=FALSE, ext= ext)
    }else{
        ext='init'
        theFile <- getFilePath( bak=bak, ext= ext)
    }

    mhead=c('IS','Snow','Overland','Unsat','Sat');
    rhead=c('RiverState','SatUndRiv');
    if(pihmver >=2.5){
        msh=readmesh();
        riv=readriv();
        m=msh$size[1]
        n=riv$River$size
        x= readBin(theFile, what=numeric(), n=m*5+n*2)
        minit <- t(matrix(x[1:(m*5)],nrow=length(mhead)) ); #,nrow=length(lid))
        rinit <- t(matrix(x[(m*5+1):(m*5+n*2)],nrow=length(rhead)) ); #nrow=length(a)-length(lid))   
        ret= list('minit'=minit, 'rinit'=rinit)
 }else{
    tline <- readLines(theFile);
    m=readmesh(shp=FALSE)$size[1]
    minit=as.matrix(read.table(text=tline[(1:m)], header=FALSE))
    colnames(minit)=toupper(mhead)
    ret= list('minit'=minit)
    if(RIVERON){
        rinit=as.matrix(read.table(text=tline[-(1:m)], header=FALSE)) 
        colnames(rinit)=toupper(rhead)
        ret = c(ret,'rinit'=rinit)
    }
    
 }
    return(ret);
}
#============ #============
#============ #============
readibc <- function(bak=FALSE){
    message('Error: uncompleted function.');
    return(ibc);
}
#============ #============
#============ #============


#============ #============
#============ #============
readinTable <- function(ext, ncol, head,bak=FALSE){  #Basic function for read table for PIHM input. Applied for soil/geol//lc
theFile= getFilePath(ext=ext, bak=bak);
    if (pihmver ==2.4){
    head=scan(theFile,what=character(),nlines=1,quiet = TRUE,blank.lines.skip = TRUE);
    head[1]='IND';
    }
mat <-t( matrix (scan(theFile,what=numeric(),skip=1,blank.lines.skip = TRUE,quiet = TRUE), nrow=ncol))
colnames(mat)=toupper(head);
return(mat);
}

#============ #============
#============ #============
readsoil <-function(bak=FALSE){
    if (pihmver >2.4){
        theFile = getFilePath(ext='soil', bak=bak)
        lines=readLines(theFile, skipNul=TRUE)
        n=as.numeric(read.table(text=lines[1], row.names=1, header=FALSE))
        x=read.table(text=lines[2:(n+2)], header=TRUE)

        y=as.matrix(read.table(text=lines[(n+3):length(lines)], row.names=1, header=FALSE))
        yy=as.numeric(y);
        names(yy)=rownames(y);

    }else if (pihmver==2.4){
        theFile = getFilePath(ext='soil', bak=bak)
        lines=readLines(theFile, skipNul=TRUE)
        x=as.matrix(read.table(text=lines, header=TRUE ))
        yy=NA;

    }else{
        theFile = getFilePath(ext='soil', bak=FALSE)
         lines=readLines(theFile, skipNul=TRUE)
        cname=c("INDEX", "INFK", "MAXSMC", "MINSMC", "DINF", "ALPHA", "BETA", "MACVF", "SATMACVK");
        x = as.matrix(read.table(text=lines[-1], col.names=cname)        )
        yy=NA; 
         x[,c(2,9)]=x[,c(2,9)]/86400;  #unit to m/s

    }
        ret =list( 'soil'=x, 'other'=yy)
        return(ret);

}

#============ #============
#============ #============
readgeol <-function(bak=FALSE){
    if (pihmver >=2.4){
        theFile = getFilePath(ext='geol', bak=bak)
        lines=readLines(theFile, skipNul=TRUE)
        #n=as.numeric(read.table(text=lines[1], row.names=1, header=FALSE))
        x=as.matrix(read.table(text=lines[1:length(lines)], header=TRUE))
        
    }else {
        theFile = getFilePath(ext='geol$', bak=FALSE)
         lines=readLines(theFile, skipNul=TRUE)
        cname=c("INDEX", "SATHK","SATVK", "MAXSMC", "MINSMC",  "ALPHA", "BETA",
                "MACHF", "SATMACKH", "DMAC");
        x = as.matrix(read.table(text=lines[-1], col.names=cname)        )
        x[,c(2:3,9)]=x[,c(2:3,9)]/86400;  #unit to m/s
    }
    yy=NA; 
    ret =list( 'geol'=x, 'other'=yy)
    return(ret);
}

#============ #============
#============ #============
readlc <-function(bak=TRUE,path=outpath){
    theFile = getFilePath(path=path, ext='lc', bak=bak)
    tl = readLines(theFile, skipNul=TRUE)
    if ( grepl('^NUM', tl[1]) ){
    NumLC=unlist(read.table(text=tl[1], header=FALSE, row.names=1))
    }else{
        NumLC=unlist(read.table(text=tl[1], header=FALSE))
    }
    
    lrange=2:(min(NumLC+2, length(tl)) )
    if( grepl('[:Alpha:]', tl[2]) ){
        header=TRUE}
    else{
        header=FALSE
    }
    tmp=as.matrix(read.table(text=tl[lrange], header=header))
    if(NumLC+1 < length(tl)){
        other=as.matrix(read.table(text=tl[-(1:(NumLC+2))], header=FALSE, row.names=1))
    }else{
        other=NA
    }
    if(ncol(tmp) ==8){
        tmp[,7]=tmp[,7]*86400;
        colnames(tmp)=c('INDEX','LAIMAX','RS','RGL','ALBMAX','SHDFAC','ROUGH','DROOT')
    }
    lc=list('data'=tmp, 'other'=other)
    return(lc);
}

#============ #============
#============ #============
showmeKs <- function( calib.bak=FALSE,quiet=FALSE,path=Resultpath){
    soil<-readsoil()$soil;
    geol<-readgeol()$geol;
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
        
        cKsoil[,'INFK'] = cKsoil[,'INFK']*calib['KINF'];
        
        id=2
        cKsoil[,id] = cKsoil[,id]*calib['KMACSATV'];

        id=1 #which(grepl('^sathk',tolower(colnames(geol))) )
        cKgeol[,id] = cKgeol[,id]*calib['KSATH'];

        id=2 #which(grepl('^satvk|^satdk',tolower(colnames(geol))) )
        cKgeol[,id] = cKgeol[,id]*calib['KSATV'];
        id=3 #which(grepl('^satmac',tolower(colnames(geol))) );
        cKgeol[,id] = cKgeol[,id]*calib['KMACSATH'];

        riv=readriv(bak=TRUE);
        nr=nrow(riv$Material$mat);
        if (nr>1){
            Krivm=riv$Material$mat[,4:5];
            
            id=which(grepl('^krivh',tolower(names(calib))) )
            Krivm=cbind(Krivm,Krivm[,1]*calib[id]);
            id=which(grepl('^krivv',tolower(names(calib))) )
            Krivm=cbind(Krivm,Krivm[,2]*calib[id]);
            colnames(Krivm)=c(colnames(Krivm[,1:2]), paste('C_',colnames(Krivm[,1:2]),sep=''));
            if(nrow(Krivm)>1){
                Krivm=rbind(Krivm,colMeans(Krivm));
                row.names(Krivm)=c(paste(1:nr),'mean');
            }
        }else{
            Krivm=riv$Material$mat[,4:5];
            
            id=which(grepl('^krivh',tolower(names(calib))) )
            Krivm=c(Krivm,Krivm[1]*calib[id]);
            id=which(grepl('^krivv',tolower(names(calib))) )
            Krivm=c(Krivm,Krivm[2]*calib[id]);
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
        cKsoil['INFK'] = cKsoil['INFK']*calib['KINF'];
        id=2
        cKsoil[id] = cKsoil[id]*calib['KMACSATV'];

        id=1 #which(grepl('^sathk',tolower(colnames(geol))) )
        cKgeol[id] = cKgeol[id]*calib['KSATH'];

        id=2 #which(grepl('^satvk|^satdk',tolower(colnames(geol))) )
        cKgeol[id] = cKgeol[id]*calib['KSATV'];
        id=3 #which(grepl('^satmac',tolower(colnames(geol))) );
        cKgeol[id] = cKgeol[id]*calib['KMACSATH'];
        
        riv=readriv(bak=TRUE);
        nr=nrow(riv$Material$mat);
        if (nr>1){
            Krivm=riv$Material$mat[,4:5];
            
            id=which(grepl('^krivh',tolower(names(calib))) )
            Krivm=cbind(Krivm,Krivm[,1]*calib[id]);
            id=which(grepl('^krivv',tolower(names(calib))) )
            Krivm=cbind(Krivm,Krivm[,2]*calib[id]);
            colnames(Krivm)=c(colnames(Krivm[,1:2]), paste('C_',colnames(Krivm[,1:2]),sep=''));
            if(nrow(Krivm)>1){
                Krivm=rbind(Krivm,colMeans(Krivm));
                row.names(Krivm)=c(paste(1:nr),'mean');
            }
        }else{
            Krivm=riv$Material$mat[,4:5];
            
            id=which(grepl('^krivh',tolower(names(calib))) )
            Krivm=c(Krivm,Krivm[1]*calib[id]);
            id=which(grepl('^krivv',tolower(names(calib))) )
            Krivm=c(Krivm,Krivm[2]*calib[id]);
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
    print(calib)
}


#============ #============
#============ #============
readmesh <-function(bak=FALSE, file=getFilePath(ext='mesh',bak=bak), shp=TRUE){
theFile= file

    lines <- readLines(theFile, skipNul=TRUE);
    if (pihmver >=2.5 ){
        tmp = scan(text=lines[1], what=character(),quiet = TRUE);
        ncell = as.numeric(tmp[2]); 
        mshhead=scan(text= lines[2],what=character(),nlines=1,blank.lines.skip = TRUE,quiet = TRUE);
        tmp = lines[3:(2+ncell)]
        msh <-t( matrix (scan(text=tmp,,what=integer(),nlines=ncell,blank.lines.skip = TRUE,quiet = TRUE), ncol=ncell))
        
        tmp = scan(text=lines[ncell+2+1], what=character(),quiet = TRUE);
        npt = as.numeric(tmp[2]); 
        mshhead=scan(text= lines[ncell+2+2 ],what=character(),nlines=1,blank.lines.skip = TRUE,quiet = TRUE);
        tmp = lines[(ncell+2+3):(ncell+2+2+npt)]
        pt <-t( matrix (scan(text=tmp,,what=numeric(),nlines=npt,blank.lines.skip = TRUE,quiet = TRUE), ncol=npt))
     
    } else if (pihmver >=2.4 ){
        
        ncell=scan(theFile,what=integer(),nmax=1,blank.lines.skip = TRUE,quiet = TRUE);
        msh = as.matrix(read.table(text=lines[1:(ncell+1)], header=TRUE))
        colnames(msh)=c('INDEX',colnames(msh)[-1]);
#        mshhead=scan(theFile,what=character(),nlines=1,blank.lines.skip = TRUE,quiet = TRUE);
#        msh <-t( matrix (scan(theFile,what=integer(),skip=1,nlines=ncell,blank.lines.skip = TRUE,quiet = TRUE), ncol=ncell))
#        colnames(msh)=c('INDEX',mshhead[-1]);
        
        
        npt=scan(theFile,what=integer(),nmax=1,skip=ncell+1,blank.lines.skip = TRUE,quiet = TRUE);
#        pthead=scan(theFile,what=character(),nlines=1,skip=ncell+1,blank.lines.skip = TRUE,quiet = TRUE);
#        pt <-t( matrix (scan(theFile,what=double(),skip=ncell+2,nlines=npt,blank.lines.skip = TRUE,quiet = TRUE), ncol=npt))
#        colnames(pt)=c('INDEX',pthead[-1]);
        
        pt = as.matrix(read.table(text=lines[-(1:(ncell+1))], header=TRUE))
        colnames(pt)=c('INDEX',colnames(pt)[-1]);

    }
    else{
        num=scan(text=lines[1],what=integer(),nmax=2,blank.lines.skip = TRUE,quiet = TRUE);
        ncell=num[1];
        npt=num[2];
        msh <- as.matrix(read.table(text = lines[(1:ncell)+1], header=FALSE))
        pt <- as.matrix(read.table(text = lines[(1:npt)+1+ncell], header=FALSE))
        #mesh<- t( matrix (scan(theFile,what=integer(),skip=1,nlines=ncell,blank.lines.skip = TRUE,quiet = TRUE), ncol=ncell))
        #pt <-t( matrix (scan(theFile,what=double(),skip=ncell+1,nlines=npt,blank.lines.skip = TRUE,quiet = TRUE), ncol=npt))
        colnames(msh) <- c("ID","NODE1","NODE2","NODE3","NABR1","NABR2","NABR3")
        colnames(pt) <- c("ID", "X","Y","ZMIN","ZMAX");

    }
    
    mesh <-list("size"=c(ncell,npt),"mesh"=msh, "points"=pt 
                );
    if(shp){
        spp= Mesh2Shp (mesh = mesh)
        mesh <-list("size"=c(ncell,npt),"mesh"=msh, "points"=pt,
                    'shp'=spp);
    }
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
    
    soil=readsoil()$soil;
    geol=readgeol()$geol;
    
    sp=soil[soilid,3];
    gp=geol[geolid,4];

    poro=matrix(c(sp,gp),nrow=2);
    rownames(poro)=c('Soil_Poro','Geol_Poro');

    return(poro);
}

readlai <- function(fn=paste0(projectname,".lai$") ){
       theFile=list.files(inpath,pattern=fn,full.names=TRUE);
    if (!file.exists(theFile)){
        stop("Error: file does not exist\n\t",theFile, "\n");
    }
    tlines <- readLines(theFile, skipNul=TRUE);    
    rids=which(unlist(lapply(tlines, FUN=function(x) grepl('^LAI_TS', x))))
    nLAI=length(rids)
    rids = c(rids, length(tlines))
    
    for (i in 1:nLAI){
        message('Parsing ',i,'/',nLAI,' LAI/RL set\n');
        
        str <- unlist(strsplit(tlines[rids[i]],' +'));
        r0 = rids[i]+3   #start row
        r1 = rids[i+1]-1    #end row
        rid = c(r0, (r0+2):r1)
        nlines = length(rid)
        tmplist=scan(text=tlines[rid],n=nlines * 4 ,
                     what=list('character','character', 'numeric','numeric'),
                     quiet = TRUE)
        t <- as.POSIXct(paste(unlist(tmplist[[1]]),unlist(tmplist[[2]])),format='%Y-%m-%d %H:%M',tz='UTC');
            if (i==1){
                lai   <- xts(as.numeric(unlist(tmplist[3])),order.by = t);
                rl    <- xts(as.numeric(unlist(tmplist[4])),order.by = t);

            }else{
                lai   <- cbind( lai , xts(as.numeric(unlist(tmplist[3])),order.by = t))
                rl    <- cbind( rl   , xts(as.numeric(unlist(tmplist[4])),order.by = t))
           }
    }
    
        lairl<-list(
                'LAI'  = lai ,
                'RL'=	rl    ,
                'nLAI'=nLAI);
        rdsfile=file.path(inpath,paste(projectname,".lairl.RData",sep=""));
        saveRDS(lairl,file=rdsfile,compress=TRUE);
        return(lairl)
}

readveg <- function(path = dirname(inpath), fn=paste0("vegprmt.tbl$") ){
    theFile=list.files(path,pattern=fn,full.names=TRUE);
    if (!file.exists(theFile)){
        stop("Error: file does not exist\n\t",theFile, "\n");
    }
    tlines <- readLines(theFile, skipNul=TRUE);    
    nl = unlist(read.table( text = tlines[1] ))[2]
    rid = 2:(nl+2)
    veg = read.table(text= tlines[rid], header=TRUE)
    
    rid = (nl+3):(length(tlines))
    x= read.table(text = tlines[rid], header=FALSE, row.names=1)
    ret = list(TBL= veg, Misc = x)
        return(ret)
}

readprj <- function (path = dirname(inpath), fn=paste0(projectname, ".prj") ){
    theFile=file.path(path,pattern=fn);
    if (!file.exists(theFile)){
        stop("Error: file does not exist\n\t",theFile, "\n");
    }
    tlines <- readLines(theFile, skipNul=TRUE);    
    nl = unlist(read.table( text = tlines[1] ))[2]
    ret =as.matrix(read.table(text=tlines, row.names=1)) 
    colnames(ret)= c('Path')
    return(ret)
}


meshAtt <- function(mesh=readmesh(bak=TRUE), att=readatt(bak=TRUE),
                    soil=readsoil(bak=TRUE),
                    geol=readgeol(bak=TRUE),
                    lc=readlc(bak=TRUE),
                    calib=readcalib(bak=TRUE)
                    ){
    tab= mesh$mesh
    s.tab = soil$soil[att[,2], ]
    g.tab = geol$geol[att[,3], ] 
    lc.tab = lc$data[att[,4], ]
    colnames(s.tab) = paste0('SOIL.',colnames(soil$soil) )
    colnames(g.tab) = paste0('GEOL.',colnames(geol$geol) )
    colnames(lc.tab) = paste0('LC.',colnames(lc$data) )
    
#    cb=calib
#    s.tab = soil$soil[att[,2], ]
#    cbname=names(cb)
#   s.tab[,'INFK'] = cb[which(grepl('INFK', cbname) | grepl('KINF', cbname))] *        s.tab[,'INFK']  
#   s.tab[,'DINF'] = cb['DINF'] *        s.tab[,'DINF']  
#    s.tab[,'MAXSMC'] = cb['POROSITY'] *   s.tab[,'MAXSMC']  
#   s.tab[,'ALPHA'] = cb['ALPHA'] *       s.tab[,'ALPHA']  
#   s.tab[,'BETA'] = cb['BETA'] *        s.tab[,'BETA']  
#   s.tab[,'MACVF'] = cb['MACVF'] *       s.tab[,'MACVF']  
#   s.tab[,'SATMACVK'] = cb['KMACSATV'] *    s.tab[,'SATMACVK']  
#    colnames(s.tab) = paste0('SOIL.',colnames(soil$soil) )
#
#   
#    g.tab[,'SATHK'] = cb['KSATH'] *        g.tab[,'SATHK']  
#    g.tab[,'SATVK'] = cb['KSATV'] *        g.tab[,'SATVK']  
#    g.tab[,'MAXSMC'] = cb['POROSITY'] *        g.tab[,'MAXSMC']  
#   g.tab[,'ALPHA'] = cb['ALPHA'] *       g.tab[,'ALPHA']  
#   g.tab[,'BETA'] = cb['BETA'] *        g.tab[,'BETA']  
#   g.tab[,'MACHF'] = cb['MACHF'] *       g.tab[,'MACHF']  
#   g.tab[,'SATMACKH'] = cb['SATMACKH'] *    g.tab[,'SATMACKH']    
#   g.tab[,'DMAC'] = cb['DMAC'] *    g.tab[,'DMAC']    
#    colnames(g.tab) = paste0('GEOL.',colnames(geol$geol) )
#
#   
#    lc.tab = lc$data[att[,4], ]
#    lc.tab[,'SHDFAC'] = cb['VEGFRAC'] *        lc.tab[,'SHDFAC']  
#    lc.tab[,'ALBMAX'] = cb['ALBEDO'] *        lc.tab[,'ALBMAX']  
#    lc.tab[,'ROUGH'] = cb['ROUGH'] *        lc.tab[,'ROUGH']  
#    lc.tab[,'DROOT'] = cb['DROOT'] *        lc.tab[,'DROOT']  
#    colnames(lc.tab) = paste0('LC.',colnames(lc$data) )
    ret = cbind(tab, s.tab, g.tab, lc.tab)
    return(ret)
}



