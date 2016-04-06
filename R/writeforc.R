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

press=prcp*0;
NumMeteoTS=length(colid)
t1=as.POSIXct('2000-01-01')
t2=as.POSIXct('2010-01-01')
years=year(t1):year(t2)


 	forcnames = c( "Precip", "Temp", "RH", "Wind", "RN", 
                   "G","VP", "LAI", "MF", "SS" )
    theFile <- file.path(path, filename);
    bakFile <- file.path(path, paste(projectname,".",'forc.',as.character(Sys.time()),sep=''));
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
    message('not ready');
    return(0);
           Forcing<-list(
                'PRCP'  =	prcp    ,
                'SFCTMP'=	temp    ,
                'RH'    =	rh  ,
                'SFCSPD'=	winds   ,
                'SOLAR' =	solar   ,
                'LONGWV'=	longw   ,
                'PRES'  =	press ,
                'NumMeteoTS'=NumMeteoTS);
 
    theFile <- file.path(path, filename);
    bakFile <- file.path(path, paste(projectname,".",'forc.',as.character(Sys.time()),sep=''));
    file.copy(theFile,bakFile)

    file.create(file=theFile);
    
    
    
    write.table(x=att,file=theFile,col.names=TRUE,row.names=FALSE,quote=FALSE)
}

