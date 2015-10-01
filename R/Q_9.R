#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 

Q9 <- function (){
    n=9
    qlist=list();
    
    for (i in 0:n){
        message(i,'\n');
        ext=paste('rivFlx',i,sep='');
        
        x=readout(ext=ext);
        qlist[[i+1]]=x
    }
    names(qlist)=paste('rivFlx',c(0:n),sep='') 
    
    return(qlist);
}
get2Q <- function(outlets=getoutlets()){
    Q=readout('rivFlx1');
    q=Q[,outlets]
    tq=as.Date(time(q));
    
    obs=readUSGSQ(getsiteid());
    to=time(obs);
    to=as.Date(to);
    
    ct=tq[tq %in% to]
    
    oq=obs[ct];
    q=q[ct];
    
    time(oq)=ct;
    time(q)=ct;
    ret=cbind(q,oq);
#    ret=list('q'=q, 'oq'=oq, 'ct'=ct)
    return(ret);
    
    
}

getQ19 <- function(outlets=getoutlets()){
    Q=readout('rivFlx1');
    q=Q[,outlets]
    Q9=readout('rivFlx9');
    q9=Q[,outlets];
       ret=cbind(q,q9);
#    ret=list('q'=q, 'oq'=oq, 'ct'=ct)
    return(ret);
}
