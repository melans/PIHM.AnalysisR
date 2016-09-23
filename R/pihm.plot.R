#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  <- ============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#'
pihm.plot <- function(x,fn='pihm.plot.png',if.save=TRUE,ylab='Y',xlab='X'){ 
    wd=1600;
    ht=1600;
    

     imgfile=file.path(Resultpath,fn)
    if (if.save && grepl('.png$',tolower(fn)) ){
        png(imgfile,width=wd, height=ht)
    }
    if (if.save && grepl('.jpg$',tolower(fn)) ){
        jpg(imgfile,width=wd, height=ht)
    }
    if (if.save && grepl('.eps$',tolower(fn)) ){
        eps(imgfile,width=wd, height=ht)
    }
    
    matplot(x,,type='l',ylab=ylab,xlab=xlab);
    
    if (if.save){
        dev.off();
    }

}
plotzoo <- function(x,y, screen=1,color,format='png',fn='plotzoo.png', if.save=TRUE,ylab='',unit='', holdon=FALSE){
    if (if.save && grepl('^png',tolower(format)) ){
        if(exists('Resultpath')){
            imgfile=file.path(Resultpath,fn)
        }else{ 
            imgfile=file.path('./',fn)
        }
        png(imgfile,width=40, height=30,unit='cm',res=100)
    }
    if(missing(y)){
        ncol=ncol(x);
        if(missing(color)){
            color <- rainbow(ncol) # assign colors to heights for each point
        }


        plot.zoo(x,screen=screen,col=color,ylab=paste(ylab,'(',unit,')'),xlab='Time');
    }else {
        ncol=ncol(y);
        if(missing(color)){
            color <- rainbow(ncol) # assign colors to heights for each point
        }
        plot.zoo(x=x,y=y,screen=screen,col=color,ylab=paste(ylab,'(',unit,')'),xlab='Time');
    }


    if (if.save){
        if(!holdon){
        dev.off();
        }
    }
    
}
pihm.hydrograph <- function(flow,precip,fn='hydrograph.png',P.units = "H/T", S.units = "V/T"){
    #P is np*1 TS
    #Q is nq*mq TS
    Q=flow;
    t=time(flow);
    nq=ncol(flow);
    p=precip[t];
    
#    hydrograph(input = matrix(ncol = 2, nrow = 2), streamflow = input[, 2],
#     timeSeries = input[, 1], streamflow2 = NULL, precip = NULL, begin = 1,
#     endindex = length(streamflow), P.units = "", S.units = "",
#     S1.col = "black", S2.col = "red", stream.label = "Streamflow",
#     streamflow3 = NULL, streamflow4 = NULL, precip2 = NULL)
    
    imgfile=file.path(Resultpath,fn)
    nc=nchar(fn);
    ftype=substr(fn,nc-2,nc);


    image.control(fn=fn);
    if (nq==1){
    hydrograph(timeSeries=t,
               streamflow=as.numeric(Q),
               precip=as.numeric(p),
               P.units=P.units,
               S.units=S.units,
               )
    }

    if (nq>1){
        c=c('streamflow=Q[,1]',paste('streamflow',2:nq,'=','Q[,',2:nq,']',sep='') );
        strflow=paste(c,collapse=', ');    
        str=paste('hydrograph(timeSeries=t',strflow,  'precip=p)',sep=', ');
        eval(parse(text=str));
    }
    dev.off();
}
pihm.matrixplot <- function(x, FUN=mean,value.name='Value', maintitle=''){
    m <- daily2monthly(x, FUN=FUN, na.rm=TRUE)
    M <- matrix(m, ncol=12, byrow=TRUE)
    colnames(M) <- month.abb
    rownames(M) <- unique(format(time(m), "%Y"))
    matrixplot(M, ColorRamp=value.name,main=)    
    
#    matrixplot(M, ColorRamp="Precipitation",main="Monthly precipitation at San Martino st., [mm/month]")
}

pihm.hydroplot <- function(x, FUN,fn='HydroPlot.png', if.save=TRUE,ylab='Y',var.unit=''){
    yr=year(x);
    period=diff(range(yr))
    
    if (period>1){
        if(if.save){
        image.control(fn=fn,path=Resultpath);    
        }
        hydroplot(x, FUN=FUN, ylab= ylab, var.unit = var.unit)
        if (if.save){
            dev.off();
        }
    }else{
        warning('Skip ploting request due to less than 2 year data\n Period =',diff(range(time(x)) ),'days\n' );
    }
}

pihm.barplot <- function(fn,data,col,ylab='',title='',ylines,ycolor='red'){
    image.control(fn=fn );    
    nc=ncol(data);
    if(missing(col)){
        col=rainbow(nc);
    }
    df.bar<-barplot(data,legend=colnames(data),col=col,ylab=ylab,main=title);
    grid();
    if (!missing(ylines)){
        lines(x=df.bar, y=ylines, col=ycolor);
    }
    dev.off();
}




