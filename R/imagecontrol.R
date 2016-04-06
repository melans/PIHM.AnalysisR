#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#'
image.control <- function(fn='image.png', path=Resultpath,wd=25,ht=20, units="cm", res=160,
                         bg='transparent', if.save=TRUE){
    if(!if.save){
        return(0)
    }
    pihm.dev.close()
    if( !file.exists(path) ){ dir.create(path)};
    imgfile=file.path(path,fn)
    nc=nchar(fn);
    
    ftype=substr(fn,nc-2,nc);   #extension of file name, which will be cmd for next line.
    if (grepl('jpg',tolower(ftype) )){
        ftype='jpeg'
    }
    cmd=paste(ftype,"(imgfile,width=wd, height=ht,units=units, res=res, bg=bg)")
        eval(parse(text=cmd)); 
    
}
image.off <- function(if.save=TRUE){
    if(if.save){
        pihm.dev.close()
    }
}

pihm.dev.close <- function(){
    dl <- dev.list();
    nd = length(dl)
    if (nd >0){
        for(i in 1:nd){
            dev.off();
        }
    }
}
imagecontrol=image.control  #alias
imageoff=image.off


