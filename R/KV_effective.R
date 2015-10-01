#
ks=1 /86400 #m/day => m/s
km=10 /86400  #m/day => m/s
alpha= 2;
beta=1.3

EffectiveSat <- function(Yunsat,deficit){
    se = Yunsat/deficit;
    se[which(se>=1)]=1
    
    return(se)
}

VGt2h <- function(se, alpha, beta, minpsi=-70){
    # Van Genutchen Theta to H
    tmp =(1/se)^(beta/(beta-1)) -1
    tmp[which(tmp<1e-13)]=0;
    h = ( tmp ) ^ (1/beta) / (-1 * alpha) ;
    h[which(h < minpsi)]= minpsi
    return(h);
}
UnsatGriadient <- function(se, alpha, beta, Ysurf, InfD=0.1){
    G = ( Ysurf - VGt2h(se, alpha, beta) )/InfD
    return(G)
}
satKfunc <- function(se, beta){
    satFK=se^0.5 * (-1 + (1-se ^(beta / (beta -1)))^ ( (beta-1) / beta) ) ^2
    return (satFK)
}
KVs <- function(Yu=seq(1, 5, length.out=1000), De=5, ks, km, alpha, beta,
                af=0.01, Ysurf=0.01, if.save=FALSE,
                fn=paste(projectname,'KV_eff.png',sep=''),if.plot=TRUE){
    se= EffectiveSat( Yunsat=Yu, deficit=De)
    ns=length(se)
    if (ns>1){        
        K=numeric(ns);
        Grad=numeric(ns)
        Psi = numeric(ns)
        status =numeric(ns)
        for(i in 1:length(se)){
            Grad[i] = UnsatGriadient(se=se[i], alpha=alpha,beta=beta, Ysurf=Ysurf)
            K[i]= EffKV(se=se[i], ks=ks, km=km, alpha=alpha, beta=beta, af=af,
                        griadient=Grad[i])
            status[i]=macpore_status(se=se[i], ks=ks, km=km, alpha=alpha, beta=beta, af=af,
                        griadient=Grad[i])

            Psi[i]= VGt2h(se[i], alpha, beta)
        }
    }else{
        Grad = UnsatGriadient(se=se, alpha=alpha,beta=beta, Ysurf=Ysurf)
        K= EffKV(se=se, ks=ks, km=km, alpha=alpha, beta=beta, af=af, griadient=Grad)
    }
    
    q=Grad * K
    ret=cbind(se, K,Grad, Psi, q, status)
    
    if (ns >1 && if.plot){
        
        if (if.save){
            imagecontrol(fn=fn, wd=25,ht=25)
        }
        library(ggplot2)

#        library(plotrix)
#        twoord.plot(se, K, se, Grad, ylab='K', rylab='Grad', xlab='Effective Saturation' )
        info=t(matrix(c('Ksat=',ks, 'Kmp=',km,'Deficit=', De, 'Alpha=',alpha,
                      'Beta=', beta, 'Af=', af, 'Ysurf=', Ysurf), nrow=2))
        leglines=paste(info[,1], info[,2])
        par(mfrow=c(2,2))
        xlab='Effective Saturation'
        
         library(plotrix)
        twoord.plot(se, K, se, Grad,ylog=TRUE, ylab='K [L/T]', rylab='Grad [L/L]', xlab='Effective Saturation' )
        #plot(se, K, xlab=xlab, ylab='K [L/T]')
        plot(se, Psi, ylog=TRUE, xlab=xlab, ylab='Matrix potential {L]')
      #  plot(se, q, xlab=xlab, ylab='Flow Rate[L/T]');

        twoord.plot(se, q, se, status, ylab='Flow Rate [L/T]', rylab='Status [L/L]', xlab='Effective Saturation' )
        plot.new()
        
        legend('topleft',leglines,bg="transparent")
#        plot(se, K, lty=1, pch=1, col=rainbow(1))
#        lines(se,Grad,lty=4, col='blue');

        if (if.save){
            dev.off()
        }
      #  plot(se, q, ylab='Flow Rate[L/T]', xlab='Effective Saturation')
    }
    return(ret)
}

EffKV <- function (se,ks, km, alpha, beta, af, griadient){
    kf= (se)^0.5 * (-1+( 1- se^(beta/(beta-1))) ^  ((beta -1)/beta) )^2 ;

    kk= km * af + ks * (1- af) * kf

    if (se >0.98 ){
        ke= kk
    }else{
        if (griadient * kf * ks < 1 * ks *kf ){
            ke= ks * kf
        }else{           
            if (griadient < kk / (ks * kf) ){
                ke = km * af * se + ks * (1- af) * kf;
            }else{
                ke = kk;
            }
        }
    }
    return(ke);
}
macpore_status <- function (se,ks, km, alpha, beta, af, griadient){
    kf= (se)^0.5 * (-1+( 1- se^(beta/(beta-1))) ^  ((beta -1)/beta) )^2 ;

    kk= km * af + ks * (1- af) * kf

    if (se >0.98 ){
       status=1 
    }else{
        if (griadient * ks *kf < 1 * ks *kf){
            status=2
        }else{           
            if (griadient < kk / (ks * kf) ){
                status=3
            }else{
                status=4;
            }
        }
    }
    return(status);
}

RechargeRate <- function(alpha, beta, Yu, Yg, ks,km, sd, af){
    Yg=9.9
    sd=10;
    Yu=sd-Yg
    af=0.01
    alpha=2;
    beta=1.2

    KS=1.2; #m/day
    KM=1600;    #m/day
    T=100;
    ufactor=86400;
    
    n=1000;
    mat=matrix(0,T,3);
    ks=KS/n;
    km=KM/n;
    
    for (i in 1:T){
        
        de= sd-Yg;
        se=EffectiveSat(Yu, de);
        griadient = 0-VGt2h(se=se,alpha=alpha, beta=beta) / 0.1;
        ke= EffKV(se=se, ks=ks, km=km, alpha=alpha, beta=beta, af=af,griadient=griadient);
       # cat('se=',se,'\n');
        fx= (  ks*Yg + ke*de ) * (alpha * de - 2* (se^(1/(1-beta)-1 ) )^(1/beta) )
        fx= fx / (alpha * (de + Yg) ) ^2;
        mat[i,1]=Yu+Yg;
        mat[i,2]=Yg;
        mat[i,3]=fx;
        
        
        Qgw =ks*rnorm(1,5)
        Eloss=rnorm(1,5) * ks
        Yu=Yu-fx+ Eloss ;
        Yg=Yg+fx- Qgw;
    }
    colnames(mat)=c('Unsat', 'GW','Rech')

    id=1:3;
    leg=colnames(mat)[id]
    matplot(mat[,id],type='l', lty=id,col=rainbow(length(id)))
    legend('right', leg,  lty=id,col=rainbow(length(id)))


}
