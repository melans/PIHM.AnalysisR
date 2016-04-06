Fun.recharge <- function(zs=10, s1=2,s2=5, alpha=3.5, beta=1.4, ke=10){
    deficit = zs - s2;
    sfc = deficit * (1+ (alpha * deficit)^(-beta) )^(-1/beta)/(alpha * deficit ) ;
    if( deficit <= s1 || sfc >=deficit){   #saturated
        grad=1 ;    # unit gradient
    }else{
        grad = (s1 - sfc)/(deficit - sfc) - s1/deficit ;
    }
    flux = ke * grad;
    ret=matrix(c(flux,grad,sfc));
    rownames(ret)=c('flux','Grad','FC');
    return(ret)
}
K4Recharge <- function(zs=10, s1=2,s2=5, ks,km,alpha=3.5,af=0.01, beta=1.4,
                       mpbeta=1.2,Dmac=1){
    de = zs - s2;
    se=EffectiveSat(s1, de);
    griadient = 0-VGt2h(se=se,alpha=alpha, beta=beta) / 0.1;
    keUnsat= EffKV(se=se, ks=ks, km=km, alpha=alpha, beta=beta,mpbeta=mpbeta,
              af=af,griadient=griadient,newversion=TRUE);
    ke = ( s2 * ks  +  de * keUnsat ) / zs;
    ke = zs / ( s2/ks +  de / keUnsat);
    return(ke)
}
Rech.Grad <- function(zs=10, s1=2,s2=5, alpha=3.5, beta=1.4,ke=1){
    deficit = zs - s2;
    sfc = deficit * (1+ (alpha * deficit)^(-beta) )^(-1/beta)/(alpha * deficit ) ;
    if( deficit <= s1 || sfc >=deficit){   #saturated
        grad=1 ;    # unit gradient
    }else{
      #  grad =1- exp( - (s1 - sfc)/(deficit - sfc) ) ;#- s1/deficit ;
        grad =(s1 - sfc)/(deficit - sfc) ;
    }
    flux = ke * grad;
    ret=matrix(c(flux,grad,sfc));
    rownames(ret)=c('flux','Grad','FC');
    return(ret)
}

Recharge <- function(zs=10, s1=2,s2=5, alpha=3.5, beta=1.4, ke=10){
    deficit = zs - s2;
        de= sd-Yg;
        se=EffectiveSat(Yu, de);
        griadient = 0-VGt2h(se=se,alpha=alpha, beta=beta) / 0.1;
        ke= EffKV(se=se, ks=ks, km=km, alpha=alpha, beta=beta, af=af,griadient=griadient,newversion=newversion);
       # cat('se=',se,'\n');
        fx= (  ks*Yg + ke*de ) * (alpha * de - 2* (se^(1/(1-beta)-1 ) )^(1/beta) )
        fx= fx / (alpha * (de + Yg) ) ^2;
        
    return(ret)
}




