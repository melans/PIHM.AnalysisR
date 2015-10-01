
TestEffKHplot <- function( ks=1.2, km=4200, sd=10, Dm=1, af=0.01, path='./',if.plot=TRUE,if.save=FALSE,length.out=100){
    if(Dm>sd){
        Dm=sd
    }

    Y=seq(0,sd,length.out=length.out);
    ny=length(Y);
    K = numeric(ny);
    macline=matrix(sd-Dm, ny, 1);
    surfline=matrix(sd, ny,1);
    for (i in 1:ny){
        if (Y[i]>sd-Dm) {
            if (Y[i] > sd ){
                ke= (km * Dm * af + ks * (1 - Dm * af) ) / sd 
            }else{
                ke= (km * (Y[i] - (sd -Dm)) * af + ks * (sd -Dm + (Y[i] - (sd - Dm))* (1-af) ) ) / Y[i] 
#            dh= Y[i] - (sd -Dm)
#            ke = (dh * km * af + ks *( dh * (1-af) + (sd - Dm) ) ) / Y[i];
            }
        }else{
            ke=ks;
        }
        K[i]= ke
    }
    
   if (if.plot){
        if(if.save){
           fn=paste('K_eff','_ks=',ks,'km=',km,'sd=',sd,'Dm=',Dm,'af=',af,'.png');
           imagecontrol(fn=fn, path=path);
        }
        info= paste('ks=',ks,'\nkm=',km,'\nsd=',sd,'\nDm=',Dm,'\naf=',af);
        plot(K, Y, log='',type='l',xlab='Keff (L/T)', ylab='Water Table (L) ', lwd=5)
        lines(K, surfline, col='red');
        text(K[1+2],surfline[1], 'Surface', col='red');
        lines(K, macline, col= 'blue');
        text(K[ny-2],macline[1], 'Macropore', col='blue');
        mtext(info,side=3)
        grid()
        if(if.save){
            dev.off();
        }
   }
    return(K);
}

TestEffKH <- function( ks=1.2, km=4200, sd=10, Dm=1, af=0.01,path='./',if.plot=TRUE,if.save=FALSE,length.out=100){   
    if(Dm>sd){
        Dm=sd
    }
    Y=seq(0,sd,length.out=length.out);
    ny=length(Y);
    K = numeric(ny);
    macline=matrix(sd-Dm, ny, 1);
    surfline=matrix(sd, ny,1);
    for (i in 1:ny){
        if (Y[i]>sd-Dm) {
            if (Y[i] > sd ){
                ke= (km * Dm * af + ks * (1 - Dm * af) ) / sd 
            }else{
                ke= (km * (Y[i] - (sd -Dm)) * af + ks * (sd -Dm + (Y[i] - (sd - Dm))* (1-af) ) ) / Y[i] 
            }
        }else{
            ke=ks;
        }
        K[i]= ke
    }
    
   if (if.plot){
        if(if.save){
           fn=paste('K_eff','_ks=',ks,'km=',km,'sd=',sd,'Dm=',Dm,'af=',af,'.png');
           imagecontrol(fn=fn, path=path);
        }
        info= paste('ks=',ks,'\nkm=',km,'\nsd=',sd,'\nDm=',Dm,'\naf=',af);
        plot(K, Y, log='',type='l',xlab='Keff (L/T)', ylab='Water Table (L) ', lwd=5)
        lines(K, surfline, col='red');
        text(K[1+2],surfline[1], 'Surface', col='red');
        lines(K, macline, col= 'blue');
        text(K[ny-2],macline[1], 'Macropore', col='blue');
        mtext(info,side=3)
        grid()
        if(if.save){
            dev.off();
        }
   }
    return(K);
}

EffKs <- function (ks, km, sd, Dm, af, path='./',if.plot=TRUE,if.save=FALSE){
    nks=length(ks);
    nkm=length(km);
    nsd=length(unique(sd) );
    ndm=length(Dm);
    naf=length(af);

    mat=expand.grid(ks,km,sd,Dm,af);
    mat=unique(mat)
    colnames(mat)=c('KsatH','KsatMacH','SoilDepth','MacporeDepth','MacFrac');
    n=nrow(mat);

    ncol=100;
    if (nsd<=1){
        Y=seq(0,sd,length.out=ncol);
    }else{
        Y=matrix(rep(unlist(lapply(sd,function(x) seq(0,x,length.out=ncol)) ), times=n/nsd),nrow=ncol)
    }
    
    df=matrix(0,100,n);
    for (i in 1:n){
        k=TestEffKH(ks=mat[i,1], km=mat[i,2], sd=mat[i,3], Dm=mat[i,4], af=mat[i,5], if.plot=FALSE,length.out=ncol);
        df[,i]=k;
    }
    K=df;
    if (if.plot){
        if(if.save){
           fn=paste('K_eff','.png');
           imagecontrol(fn=fn, path=path);
        }
    
 #   macline=matrix(sd-Dm, nrow=ncol, nsd);
#    surfline=matrix(sd, nrow=ncol,nsd);

           info= paste('ks=','mean','\nkm=',km,'\nsd=',sd,'\nDm=',Dm,'\naf=',af);
    matplot(K,Y,type='l',xlab='Keff (L/T)', ylab='Water Table (L) ', lwd=5)
#    legend('right',legend=paste('ks=',ks));
    
#        lines(K[,1], surfline, col='red');
#        text(K[1+2,1],surfline[1], 'Surface', col='red');
#        lines(K[,1], macline, col= 'blue');
#        text(K[ny-2,1],macline[1], 'Macropore', col='blue');
#        mtext(info,side=3)
        grid()
        if(if.save){
            dev.off();
        }
   }

    Klist=list('Para'=mat, 'Depth'=Y,'K'=K);
    return(Klist);
    
}

Ke <- function(path='./',if.plot=TRUE,if.save=FALSE,if.simple=TRUE){
    
#    if.plot=TRUE
#    if.save=FALSE
        K=Kcalib(calib.bak=TRUE,quiet=TRUE);
        calib=readcalib(bak=TRUE);
    if(if.simple){
        ks=unique(range( round(K$cKgeol[,1], ,digits=3) ) )
        km=unique(range(round(K$cKgeol[,3],,digits=3 )) )
        ssd=round(soildepth(),digits=3);
        sd=unique( range( ssd ) )
        geol=readgeol(bak=TRUE)
        Dm=unique(range(geol[,10])) * calib$value['DMAC']
        af=unique(range(geol[,8]))* calib$value['MACHF']
        
        if (length(sd)>1){
            sdq=seq(from=sd[1],to=sd[2],length.out=5)
        }else{
            sdq=sd;
        }
        kes= EffKs (ks, km, sdq, Dm, af, path=path,if.plot=if.plot,if.save=if.save)
    }else{
        att=readatt(bak=TRUE);
        gid=unique(att[,3]);
        ksm=unique(round(K$cKgeol[gid,c(1,3)] ,digits=3))
        kmx=ksm[,1];
        ksm=ksm[order(kmx),]
        
        ssd=round(soildepth(),digits=3);
        sd=unique( range( ssd ) )
        geol=readgeol(bak=TRUE)
        Dm=unique(range(geol[,10])) * calib$value['DMAC']
        af=unique(range(geol[,8]))* calib$value['MACHF']
        
        if (length(sd)>1){
            sdq=seq(from=sd[1],to=sd[2],length.out=5)
        }else{
            sdq=sd;
        }
      kes= EffKs (kms=kms, sdq, Dm, af, path=path,if.plot=if.plot,if.save=if.save)
    }
    return(kes);
    
}
