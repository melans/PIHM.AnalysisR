#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#'  <- ============================================
#' @param  inpath 
#' @param  outpath 
#' @param projectname
#' @keywords PIHM, output
#' @export  all data;
#' @return out A list of all output in outpath. out$names out$ET1 etc.
#' @examples
#' loadoutputQ(outpath,projectname,inpath)

loadoutput <-function(extlist,reload=FALSE, path=outpath, rdspath=outpath,
                      rdsname=paste(projectname,"_out.RDS",sep="")){
#   nargin <- nargs();
#    if (nargin <1){
#        cat("\nUsage:\n\t PIHMout <- loadoutput(outpath=\"./\",projectname)\n");
#        cat("\n\n");
#        return(0);
#    }
    rdsfile=file.path(rdspath,rdsname);
    fns=list.files(path=path,pattern=paste(projectname,'.*.dat',sep=''),full.names=TRUE)
    tf = max(file.info(fns)[,'mtime'])  #max time tage of modification of .dat file
    tr = file.info(rdsfile)[,'mtime'] #time tag of rds file.
    if (missing(reload)){
        if (file.exists(rdsfile)){
            if (tf-tr >0){
                reload=TRUE;  #new output files.
            }else{
                reload=FALSE; #newest out files
            }
        }else{
            reload =TRUE;    #no rds file saved.
        }
    }
    if ( ! reload){
         out = readRDS(file=rdsfile) #read from RDS file.
    }else {
        if(!missing(extlist)){
            exts<-paste(projectname,'.',extlist,'.dat$',sep='')
            pattern=paste(exts,collapse='|')
            if (pihmver >2.3){
                flist <- list.files(path=path,ignore.case = TRUE,pattern=pattern);
                fpath <- list.files(path=path,ignore.case = TRUE,pattern=pattern, full.names = TRUE );
            }else{
                flist <- list.files(path=path,ignore.case = TRUE,pattern=pattern );
                fpath <- list.files(path=path,ignore.case = TRUE,pattern=pattern, full.names = TRUE );
            }
        }
        else{
            pattern=paste(projectname,'.*.dat$',sep='')
             if (pihmver >2.3){
                flist <- list.files(path=path,ignore.case = TRUE,pattern=pattern);
                fpath <- list.files(path=path,ignore.case = TRUE,pattern=pattern, full.names = TRUE );
            }else{
                flist <- list.files(path=path,ignore.case = TRUE,pattern=pattern );
                fpath <- list.files(path=path,ignore.case = TRUE,pattern=pattern, full.names = TRUE );
            } 
        }
        dataname <- substring(flist,nchar(projectname) + 2,nchar(flist) - 4)
        
        out <-list(); # initialize the return list.
        #out <- list("names"=dataname);  #Names of data;
        length(out) <- length(dataname);
        names(out) <- dataname;
        mesh <- readmesh(bak=TRUE, file=dir(path=path, pattern=paste(projectname, '.mesh.bak', sep=''), full.names=TRUE ) );
        riv <- readriv(bak=TRUE, file=dir(path=path, pattern=paste(projectname, '.riv.bak', sep='') , full.names=TRUE ) ); 
        for( i in 1:length(fpath)){
            if (file.info(fpath[i])$size >0){
                message(fpath[i]);
                nc <- mesh$size[1] ;
                if ( grepl('^riv',tolower(dataname[i])) || grepl('^stage',tolower(dataname[i])) ) {
                    nc <- riv$River$size 
                }
                d <- readout( dataname[i],binary=TRUE,nc)
                t<-time(d)[nrow(d)];            
                n=mesh$size[[1]];
                out[[i]]=d;
                m=nrow(d);
                n=ncol(d);
                cat("Size=[",m,",",n,"]\n");
                #cmd=paste('out$',dataname[i],"=d",sep='')
                #eval(parse(text=cmd))
            }else{
                cat('File is empty. ', flist[i],'\n');
            }
        }
        t=time(out[[1]]);
        out=c(out,list('CommonTime'=t));  
        
        saveRDS(out,file=rdsfile,compress=TRUE);
    }
    valuenames=names(out)
    if ('ET0' %in%  valuenames & 'ET1' %in%  valuenames & 'ET2' %in%  valuenames ){
        out$ET=out$ET0+out$ET1+out$ET2
    }
    if ('FluxSub0' %in%  valuenames & 'FluxSub1' %in%  valuenames & 'FluxSub2' %in%  valuenames ){
        out$FluxSub=out$FluxSub0+out$FluxSub1+out$FluxSub2
    }
    if ('FluxSurf0' %in%  valuenames & 'FluxSurf1' %in%  valuenames & 'FluxSurf2' %in%  valuenames ){
        out$FluxSurf=out$FluxSurf0+out$FluxSurf1+out$FluxSurf2
    }
    if ('GW' %in%  valuenames & 'GW' %in%  valuenames ){
        out$gu=out$GW+out$unsat
    }
        assign('PIHMOUT', out,envir=.GlobalEnv);
    return(out)
}

