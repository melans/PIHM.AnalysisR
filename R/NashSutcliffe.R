#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;
NashSutcliffeEff <- function(obs, sim){
    ns=ncol(sim)
    time(obs)=as.Date(time(obs))
    to=as.Date(time(obs))
    tq=as.Date(time(sim))
    ct=tq[tq %in% to]

    oq=obs[ct]
    qq=sim[ct]
    
    if (ns>1){
        nse=numeric(ns)
        for (i in 1:ns){
            nse[i]= 1-sum((oq-qq)^2)/sum( (oq-mean(oq) )^2) 
        }
    }else{
        nse= 1-sum((oq-qq)^2)/sum( (oq-mean(oq) )^2) 
    }
    return(nse)
}
