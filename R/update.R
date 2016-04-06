#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;

updateinit <- function(n=0,fn=paste(projectname,".",'init.updateR',sep='')){
    x=readinit(bak=FALSE)
vnames=c('IS','snow','surf','unsat','GW')

for (i in 1:5){ 
    y=readout(vnames[i]);
    if (n<=0){
        x$minit[,i]= t(y[nrow(y),])
    }else{
        if (n <=nrow(y))
            x$minit[,i]= t(y[n,])
        else{
            stop('n=', n, ' is out of range (1,',nrow(y),')')
        }
    }

}
para=readpara(bak=TRUE)

if(is.null(para$RIVERSWITCH) & is.null(para$riverswitch) ){
    if.riv=FALSE;
}else{
    if(para$RIVERSWITCH>0){
        if.riv=TRUE;
    }else{
        if.riv=FALSE;
    }
}

if(if.riv){
    y1=readout('rivstage')
    y2=readout('rivGW');
     if (n<=0){

    y1=y1[nrow(y1),]
    y2=y2[nrow(y2),]
    }else{
        if (n <=nrow(y1) & n <=nrow(y2) ){          
            y1=y1[n,]
            y2=y2[n,]
        }
        else{
            stop('n=', n, ' is out of range (1,',nrow(y1),'/',nrow(y2),')')
        }
    }

}else{
    riv=readriv()
    nriv=riv$River$size
    y1=numeric(nriv)
    y2=numeric(nriv)
}
    x$rinit=cbind(y1,y2)
    writeinit(x,fn=fn)
}
