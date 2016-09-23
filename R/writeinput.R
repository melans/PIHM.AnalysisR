#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;


writeatt <-function(att,path=inpath, filename=paste(projectname,".",'att',sep='')){
    theFile <- file.path(path, filename);
    bakFile <- file.path(path, paste(projectname,".",'att.',as.character(Sys.time()),sep=''));
    file.copy(theFile,bakFile)
    file.create(file=theFile);
    message('\nWriting file ', theFile)
    write.table(x=att,file=theFile,col.names=TRUE,row.names=FALSE,quote=FALSE)
}

writecalib <- function(calib, newfilename){
    if (missing(newfilename)){
        theFile <- file.path(inpath, paste(projectname,".",'calib',sep=''));
        bakFile <- file.path(inpath, paste(projectname,".",'calib.',as.character(Sys.time()),'-',rnorm(1),sep=''));
        file.copy(theFile,bakFile)
    }else{
        theFile <- file.path(inpath, newfilename);
    }
    message('\nWriting file ', theFile)
    
    file.create(file=theFile);
    for (i in 1:length(calib$offon) ) {

        if (calib$offon[i]){
            str = paste(names(calib[i]),calib[i],calib$comments[i],sep='\t') ;
        }else{
            str = paste(paste('#',names(calib[i]),sep=''),calib[i],calib$comments[i], sep='\t')
        }
            write(x=str,file=theFile,append =TRUE)
    }

}

writemesh <- function(mesh,path=inpath, fn=paste(projectname,".",'mesh',sep='')){
    theFile <- file.path(path, fn);
   # bakFile <- file.path(path, paste(projectname,".",'mesh.',as.character(Sys.time()),'-',rnorm(1),sep=''));
    bakFile = paste(theFile, as.character(Sys.time()),'-',rnorm(1),sep='');
    file.copy(theFile,bakFile)
    message('\nWriting file ', theFile)
    
    file.create(file=theFile);
    str = c(as.character(mesh$size[1]),colnames(mesh$mesh)[-1]);
    
    write.table(x=str,file=theFile,append=FALSE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");  
    write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x=mesh$mesh,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   
    
    str = c(as.character(mesh$size[2]),colnames(mesh$points)[-1]);
    write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");   
    write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x=mesh$points,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   


}

writeinit <- function(init, fn=paste(projectname,".",'init',sep=''), path=inpath){
     theFile <- file.path(path,fn );
    bakFile <- file.path(inpath, paste(projectname,".",'init.',as.character(Sys.time()),'-',rnorm(1),sep=''));
    file.copy(theFile,bakFile)
    message('\nWriting file ', theFile)
    
    file.create(file=theFile);
    write.table(x=init$minit,file=theFile,append=FALSE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\n");  
    write.table(x=init$rinit,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\n");  
}

writeriv <- function(riv,path=inpath, fn=paste(projectname,".",'riv',sep='')){
    theFile <- file.path(path, fn);
    bakFile = paste(theFile, as.character(Sys.time()),'-',rnorm(1),sep='');
    file.copy(theFile,bakFile)
    message('\nWriting file ', theFile)
    file.create(file=theFile);
    #riv
    data= riv$River$riv
    str = c(as.character(nrow(data)),colnames(data)[-1]);
    write.table(x=str,file=theFile,append=FALSE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");  
    write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x=data,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   
    
    #shp
    data= riv$Shape$shp
    str = c(as.character(nrow(data)),colnames(data)[-1]);
    write.table(x='Shape',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");  
    write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x=data,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   
    
    #mat
    data= riv$Material$mat
    str = c(as.character(nrow(data)),colnames(data)[-1]);
    write.table(x='Material',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");  
    write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x=data,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   
    
    #ic
    data= riv$IC$ic
    str = c(as.character(nrow(data)),colnames(data)[-1]);
    write.table(x='IC',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");  
    write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x=data,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   
   
    write.table(x='BC',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x='0',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x='Res',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    write.table(x='0',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    
#
#    #Bc
#    str = c(as.character(riv$size[1]),colnames(riv$BC)[-1]);
#    write.table(x=str,file=theFile,append=FALSE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");  
#    write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
#    write.table(x=riv$BC,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   
#
#    #res
#    str = c(as.character(riv$size[1]),colnames(riv$RES)[-1]);
#    write.table(x='RES',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
#    write.table(x=str,file=theFile,append=FALSE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");  
#    write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
#    write.table(x=riv$RES,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   

}

writelai <-function(x,path=inpath, fn=paste0(projectname,".",'lai')){
    theFile <- file.path(path, fn);
    bakFile <- paste0(theFile,'.',as.character(Sys.time()))
    file.copy(theFile,bakFile)
    message('\nWriting file ', theFile)
    file.create(file=theFile);
h1 =c('TIME','LAI', 'RL')
h2=c('TS','m2/m2', 'm')
    ntype = ncol (x$LAI)
    names = colnames(x$LAI)
    if (pihmver <2.5){
        str = c('NumLAI', ntype);
        write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");
        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    }
    for (i in 1:ntype){
        message(' Type ', i ,'/',ntype, '\t', names[i]);
        str = c('LAI_TS',i);
        write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");   
        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
       str=h1 
        write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");   
        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
        str=h2
        write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");   
        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
        tm = strftime(time(x$LAI[,i]),format='%Y-%m-%d %H:%M') 
        data = data.frame(tm, x$LAI[,i], x$RL[,i])
        
        write.table(x=data,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   
#        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    }

}

writeveg <- function (x,path=dirname(inpath), fn=paste0('vegprmt.tbl')){
    theFile <- file.path(path, fn);
    bakFile <- paste0(theFile,'.',as.character(Sys.time()))
    file.copy(theFile,bakFile)
    message('\nWriting file ', theFile)
    file.create(file=theFile);
    h1 = colnames(x$TBL)
        str = c('NUMLC',nrow(x$TBL));
        write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");   
        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
       str=h1 
        write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");   
        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
        cmt=paste( '# ', rownames(x$TBL))
        data = data.frame(x$TBL, cmt)
        write.table(x=data,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   
        write.table(x=x$Misc,file=theFile,append=TRUE,col.names=FALSE,row.names=TRUE,quote=FALSE);  
}
    
writeprj <- function( x, path = dirname(inpath), fn=paste0(projectname, ".prj") ){
    theFile=file.path(path,pattern=fn);
    if( missing('x') ){
        rn =c('RIV', 'MESH', 'ATT', 'SOIL', 'GEOL', 'LC', 'METEO', 'LAI', 'BC', 'PARA',
              'CALIB', 'IC', 'LSM', 'RAD', 'CYCLES', 'SOILINIT', 'CROP')
        ret=as.matrix(c(
                paste0(path, '/', projectname,".riv"     ),
                paste0(path, '/', projectname,".mesh"    ),
                paste0(path, '/', projectname,".att"     ),
                paste0(path, '/', projectname,".soil"    ),
                paste0(path, '/', projectname,".geol"    ),
                paste0(dirname(inpath),"/vegprmt.tbl"   ),
                paste0(path, '/', projectname,".meteo"   ),
                paste0(path, '/', projectname,".lai"     ),
                paste0(path, '/', projectname,".bc"      ),
                paste0(path, '/', projectname,".para"    ),
                paste0(path, '/', projectname,".calib"   ),
                paste0(path, '/', projectname,".ic"      ),
                paste0(path, '/', projectname,".lsm"     ),
                paste0(path, '/', projectname,".rad"     ),
                paste0(path, '/', projectname,".cycles"  ),
                paste0(path, '/', projectname,".soilinit"),
                paste0(path, '/', projectname,".crop"  )
        ), ncol=1)
        colnames(ret)=c('Path')
        rownames(ret)=rn;
        x = ret
    }    
    
    bakFile <- paste0(theFile,'.',as.character(Sys.time()))
    file.copy(theFile,bakFile)

    write.table(x=x,file=theFile,append=FALSE,col.names=FALSE,row.names=TRUE,quote=FALSE);   

}



