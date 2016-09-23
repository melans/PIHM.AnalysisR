
AssessRecessionCurve <- function(obs,q, sdate, edate){
    if (missing(q)) {
        q=goQ();
    }
    t=time(q);
    tlim=range(t);
    if (missing(obs)){
        obs=readUSGSQ(getsiteid());
    }
    obs=obs[t];
    time(q)=time(obs);
    
    fn=paste('RecesionCurve',as.character(t[1]),'.png', sep='')
    image.control(fn=fn);
    ggof(sim=q,obs=obs);
    dev.off();
    fn=paste('RecesionCurve_Normalized',as.character(t[1]),'.png', sep='')
    image.control(fn=fn);
    ggof(sim=Normalize(q),obs=Normalize(obs));
    dev.off();
}


takepeak <- function(obs,ndays=7,sdate='2000-01-01',edate='2010-12-31',if.decreasing=FALSE){
# find a representative rainfall event which has a good hydrograph curve.
    if (missing(obs)){
        obs=readUSGSQ(getsiteid(),sdate=sdate,edate=edate);
    }
    no=length(obs);
    n=no-ndays;
    mat=matrix(0,n,ndays);
    t=time(obs);
    for (i in 1:n){
        ids=i:(i+ndays-1);
        mat[i,]=as.matrix(obs[ids],nrow=1);
    }
    tmp=mat;
    max=apply(tmp,1,max);
    idmax = which(max==tmp[,1])
    
    min=apply(tmp,1,min);
    idmin = which(min==tmp[,ndays]);
    

    std=apply(tmp,1,sd);
    idstd=which(std>(max(std)*2/5)) ;
   
    dx=t(apply(tmp,1,diff) );
    y=dx*0;
    y[which(dx<0)]=1;
    z=apply(y,1,sum);
    iddecr=which(z==ndays-1);
    
    id=Reduce(intersect, list(idmax,idmin,idstd))
    if(if.decreasing){
        id=Reduce(intersect, list(id,iddecr))
    }
    nid=length(id);
    ret=list(nid);
    if (nid>=1){
        if( nid==1){
            tid=unlist(lapply(id,function(x) 0:(ndays-1)+x ) )     
            tt=t[tid];
            pdata=as.xts(as.numeric(tmp[id,] ),order.by=tt ) 
            plot(pdata,type='l')
            ret[[1]]=pdata;
        }else{
            pdata=t(tmp[id,] )
            matplot(pdata,type='l')
            for(i in 1:nid){              
                tid=id[i]+(1:ndays -1);  
                tt=t[tid]
                ret[[i]]=as.xts(pdata[,i],order.by=tt);
            }
        }
       
    }else{
    }
    
    return(ret);
    
}

