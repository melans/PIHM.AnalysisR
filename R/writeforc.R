#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;


writeforc20 <-function(x,path=inpath, filename=paste(projectname,".",'forc',sep='')){
#Forcing<-list(
#                'PRCP'  =	prcp    ,
#                'SFCTMP'=	temp    ,
#                'RH'    =	rh  ,
#                'SFCSPD'=	wind  ,
#                'SOLAR' =	rn   ,
#                'LONGWV'=	lw   ,
#                'PRES'  =	press ,
#                'NumMeteoTS'=NumMeteoTS);
#        message('not ready');
#    return(0);

#press=prcp*0;
NumMeteoTS=length(x[[1]])
t1=as.POSIXct('2000-01-01')
t2=as.POSIXct('2010-01-01')
years=year(t1):year(t2)


 	forcnames = c( "Precip", "Temp", "RH", "Wind", "RN", 
                   "G","VP", "LAI", "MF", "SS" )
    theFile <- file.path(path, filename);
    bakFile <- file.path(path, paste(filename,'.',as.character(Sys.time()),sep=''));
    file.copy(theFile,bakFile)
    
    file.create(file=theFile);
    nforc=length(forcnames)
    header=c(NumMeteoTS*(numeric(5)+1), 0,0,nlai,0,0)
    write(x=header,file=theFile, append=FALSE, ncolumns=10 )
    nsites=NumMeteoTS
    
    for(i in 1:5){
        message('Printing ', forcnames[i]);
        t=time(Forcing[[i]])
        t=which(t>=t1 & t <=t2)
        dt=as.numeric(diff(t)/24); 
        dt =c(dt,dt[length(dt)]);
        nt=length(t);
        t0 = cbind(cumsum(dt)-dt, cumsum(dt))
        for( j in 1:nsites){
            message('\tsites ', j,'\t', nt);
            write(x=paste(forcnames[i], j, 2*nt),file=theFile , append=TRUE)
            dat=as.numeric(Forcing[[i]][t,j])
            data=cbind(t0[,1],dat,t0[,2] , dat);
            write(x=t(data),file=theFile , append=TRUE, ncolumns=4)
        }
    }
    lr=LaiRf(lc,years)
    nlc=length(lc)
    days=time(lr$LAI)
    cum = cumsum(as.numeric(diff(days)));
    #LAI
    message('Printing ', 'LAI');
    t=1:length(cum)
    nt=length(t)
    t0 = cbind(cum-cum[1], cum)
    for( j in 1:nlc){
        message('\tsites ', j,'\t', nt);
        write(x=paste('LAI', j, 2*nt),file=theFile , append=TRUE)
        dat=as.numeric(lr$LAI[t,j])
        data=cbind(t0[,1],dat,t0[,2] , dat);
        write(x=t(data),file=theFile , append=TRUE, ncolumns=4)
    }
    #ROUGHNESS
    i=9;
    message('Printing ', 'Roughness');
    t=1:length(cum)
    nt=length(t)
    t0 = cbind(cum-cum[1], cum)
    for( j in 1:nlc){
        message('\tsites ', j,'\t', nt);
        write(x=paste('RL', j, 2*nt),file=theFile , append=TRUE)
        dat=as.numeric(lr$Rough[t,j])
        data=cbind(t0[,1],dat,t0[,2] , dat);
        write(x=t(data),file=theFile , append=TRUE, ncolumns=4)
    }

    i = 9
    mf = MeltFactor(years)
    days=time(mf)
    cum = cumsum(as.numeric(diff(days)));
        message('Printing ', 'MeltFactor');
    t=1:length(cum)
    nt=length(t)
    t0 = cbind(cum-cum[1], cum)
    message('\t Melt Factor','\t', nt);
    write(x=paste('MF', 1, 2*nt),file=theFile , append=TRUE)
    dat=as.numeric(mf[t])
    data=cbind(t0[,1],dat,t0[,2] , dat);
    write(x=t(data),file=theFile , append=TRUE, ncolumns=4)

}

writeforcmf <-function(x,path=inpath, filename=paste(projectname,".",'forc',sep='')){
    theFile <- file.path(path, filename);
    bakFile <- paste0(theFile,'.',as.character(Sys.time()))
    file.copy(theFile,bakFile)
    file.create(file=theFile);
h1 =c('TIME','PRCP','SFCTMP','RH','SFCSPD','SOLAR','LONGWAVE','PRES')
h2=c('TS','kg/m2/s','K','%','m/s','W/m2','W/m2','Pa')
    #NumMeteoTS
    str = c('NumMeteoTS', x$NumMeteoTS);
    nsites = ncol (x$PRCP)
    if (pihmver <2.5){
        write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");
        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    }
    for (i in 1:nsites){
        message('Site ', i ,'/',nsites);
        str = c('METEO_TS',i,'WIND_LVL',10);
        write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");   
        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
       str=h1 
        write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");   
        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
        str=h2
        write.table(x=str,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE,eol = "\t");   
        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
        tm = strftime(time(x$PRCP[,i]),format='%Y-%m-%d %H:%M') 
        data = data.frame(tm, x$PRCP[,i], x$SFCTMP[,i],x$RH[,i],x$SFCSPD[,i],x$SOLAR[,i], x$LONGWV[,i],x$PRES[,i])
        
        write.table(x=data,file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);   
        write.table(x='',file=theFile,append=TRUE,col.names=FALSE,row.names=FALSE,quote=FALSE);  
    }

}

writeforc <- function( x,path=inpath, filename=paste(projectname,".",'forc',sep='') ){ 
    if (pihmver >2.2){
        writeforcmf(x=x,path=path, filename=filename)
    }else{
        writeforc20(x=x,path=path, filename=filename)
    }
}

