  

qq=cbind(q,oq);
    fv = fdc(qq)


   
   # perc=(0:(n-1))/n;
    perc = c(1, 5,(1:9)*10, 95,99)/100
    n=length(perc)
    x=fv[,1]
    d=qq[,1];
    Q=numeric(n);
    for (i in 1:n){
        oid = orderdist(x, perc[i])[1:5]
        Q[i]=mean(d[oid])
    }
    Qs=Q;
    
    x=fv[,2]
    d=qq[,2];
    Q=numeric(n);
    for (i in 1:n){
        oid = orderdist(x, perc[i])[1:5]
        Q[i]=mean(d[oid])
    }
    Qo=Q;
    
#  Ns=(Qs-mean(Qs))/mean(Qs)
#  No=(Qo-mean(Qo))/mean(Qo)
#  CE= 1- sum((Ns-No)^2/(No-mean(No))^2 )
x=as.numeric(Qs)
y=as.numeric(Qo)
LineFit(x,y,if.save=FALSE)

    CE= 100*sum( abs(Qs-Qo)  / sum(Qo) )
    CE

