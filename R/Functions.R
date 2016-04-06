#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;

Eudist<-function(pt1,pt2){
    if (length(pt1)>2){
    x1=pt1[,1];
    x2=pt2[,1];
    y1=pt1[,2];
    y2=pt2[,2];
    }else{
        x1=pt1[1];
        x2=pt2[1];
        y1=pt1[2];
        y2=pt2[2];
    }

    dist<- sqrt((x1-x2)^2+(y1-y2)^2);
    return(dist);
}
   
matRowMulti <-function(mat, vec){
    if(length(vec)!=ncol(mat)){
        stop('error of RowMulti,matrix is',dim(mat) ,'vector is',dim(vec),'\n');
    }
    a=mat;
    nr=nrow(mat);
    b=t(matrix(rep(vec,nr),ncol=nr));;
    
    ret=a*b;
    return(ret);
}


classify <- function(sq, var){
    nv=length(var)
    c=numeric(nv)
    for (i in 1:nv){
        c[i]=which.min( (sq-var[i])^2 )[1]; 
    }
    return(c);

}

Normalize <- function(x){
    y= (x-min(x))/(max(x)-min(x));
    return( y);
}

a2d <- function(a,b,c){ 
    if (length(a)==3){
        return( a[1]+a[2]/60+a[3]/3600)
    }else{
        return( a+b/60+c/3600)
    }
}

LineFit <-function(x ,y, fn,path=Resultpath,if.save=TRUE, xlab='Observation',
                   ylab='Simulation',log='xy'){
    reg1 <- lm(x~y)       
coe=coefficients(reg1)
a=round(coe[2],2)
b=round(coe[1],2)
xloc=1/3*max(x,na.rm=TRUE);
yloc=2/3*max(y,na.rm=TRUE);

    if (missing(fn)){
        fn='LineFit.png'
    }
        imagecontrol(fn, path=path, wd=25,ht=25, if.save=if.save)
   lim=range(c(x,y),na.rm=TRUE) 
   lim[2]=round(lim[2], -log10(lim[2]))
xloc=1/3*lim[2];
yloc=2/3*lim[2];


plot(x, y,asp=1, xlab=xlab,ylab=ylab,log=log, xlim=lim,
     ylim=lim,col='blue', pch=20)
abline(b,a, col='red')
abline(0,1,col='grey', lty=3,xlim=lim,ylim=lim)
grid()
eqtext=bquote(italic(Y) == .(a) *italic(X) + .(b) ) 
text(xloc,yloc, eqtext, cex = 1.2)  
clip(0,max(x,na.rm=TRUE),0, max(y,na.rm=TRUE))

    if (if.save){
        dev.off()
    }

}
mindist<-function(x,x0=0){
    oid=orderdist(x=x,x0=x0);
    return(x[oid[1]]);
}
orderdist <- function(x,x0=0,decreasing=FALSE){
    d=(x-x0)^2;
    oid=order(d,decreasing=decreasing);
    return(oid);
}


pihm.pbfdc <- function(sim,obs, perctages=c(1, 5,(1:9)*10, 95,99)/100){
    q=sim;
    oq=obs;
    qq=cbind(q,oq);
    fv = fdc(qq,plot=FALSE)
   # perctages=(0:(n-1))/n;
#    perctages = c(1, 5,(1:9)*10, 95,99)/100
    
    n=length(perctages)
    x=fv[,1]
    d=qq[,1];
    Q=numeric(n);
    for (i in 1:n){
        oid = orderdist(x, perctages[i])[1:3]
        Q[i]=mean(d[oid])
    }
    Qs=Q;
    
    x=fv[,2]
    d=qq[,2];
    Q=numeric(n);
    for (i in 1:n){
        oid = orderdist(x, perctages[i])[1:3]
        Q[i]=mean(d[oid])
    }
    Qo=Q;
#x=as.numeric(Qs)
#y=as.numeric(Qo)
#LineFit(x,y,if.save=FALSE)

    pbfdc= 100*sum( abs(Qs-Qo)  / sum(Qo) )
    return(pbfdc)
}


vector.identical <- function (set,sub, rowcol=1){
    nset=dim(set)
    nsub=length(sub);
    
    if(rowcol==1){
        if(nsub != nset[2]){
            return(FALSE)
        }
        mat=t(matrix(sub,nrow=nset[2], ncol=nset[1]))
        x=rowSums((set-mat)^2)
        return(x==0)
    }else if (rowcol==2){
        if(nsub != nset[1]){
            return(FALSE)
        }
        mat=matrix(sub,nrow=nset[1], ncol=nset[2])
        x=colSums((set-mat)^2)
        return(x==0) 
    }
}

apply.hourly <-function (x, FUN, ...) 
{
    ep <- endpoints(x, "hours")
    period.apply(x, ep, FUN, ...)
}


soilmoisture <- function(gw,unsat){
    sd=soildepth()
    d=dim(gw)
    msd=t(matrix(sd,ncol=d[1],nrow=d[2]))
    y=msd-gw
    cmc=unsat/y
    tmp=as.numeric(cmc)
    tmp[which(y<0.1)]=1
    cmc=matrix(tmp,ncol=d[2],nrow=d[1])
    
}

LinePointDistance <- function( Line,Points){
    np = length(Points)/2
    if( np ==1 ) {
        Lx1=Lines[1]
        Ly1=Lines[2]
        Lx2=Lines[3]
        Ly2=Lines[4]
        
        px=Poinst[1]
        py =Poinst[2]
        
        dist = abs( (Ly2-Ly1)*px -(Lx2-Lx1) * py + Lx2*Ly1 - Ly2 * Lx1) / sqrt( (Ly2 -Ly1)^2 +  (Lx2 -Lx1)^2 )
    }else{        
        Lx1=Lines[1]
        Ly1=Lines[2]
        Lx2=Lines[3]
        Ly2=Lines[4]
        
        dist= numeric(np)
        for (i in 1:np){
            px=Poinst[i,1]
            py =Poinst[i,2]       
            dist[i] = abs( (Ly2-Ly1)*px -(Lx2-Lx1) * py + Lx2*Ly1 - Ly2 * Lx1) / sqrt( (Ly2 -Ly1)^2 +  (Lx2 -Lx1)^2 )
        }
    }

    
    return(dist)
}


InterpSpatial <-function(data, elevation=0,ngrids=200,res){
      mesh <- readmesh(bak=TRUE);
    msh <- mesh$mesh;
    pts <- mesh$points;
    x=(pts[msh[,2],2]+pts[msh[,3],2]+pts[msh[,4],2])/3;
    y=(pts[msh[,2],3]+pts[msh[,3],3]+pts[msh[,4],3])/3;
    xlim <- range(x);
    ylim <- range(y);
    if (missing('res') ){
        dx=(xlim[2]-xlim[1])/ngrids;
    }else{
        dx=res
    }
    dy=dx; #(ylim[2]-ylim[1])/100;
    xc <- seq(xlim[1], xlim[2],by=dx);
    yc <- seq(ylim[1], ylim[2],by=dy);
    zname='Elevation'
    
    if (missing('data')){
        data= (pts[msh[,2],5]+pts[msh[,3],5]+pts[msh[,4],5])/3; #surface elevation
    }
        
    if(elevation ==0){
        z=data;
    }else if(elevation <1) {
        z=(pts[msh[,2],4]+pts[msh[,3],4]+pts[msh[,4],4])/3; #bed rock elevation
        z=z+data
    } else if( elevation >1){
        z=(pts[msh[,2],5]+pts[msh[,3],5]+pts[msh[,4],5])/3; #surface elevation
        z=z+data
    }else{
        stop('Wrong elevation parameter\n');
    }

   mat <- interp(x,y,z,xo=xc,yo=yc)   
   return(mat)
}

perpendicular <-function(p1,p2,len=0){
    tmp = p2-p1
    if( tmp[1] > 0 ){
       x1=p1[1]
       y1=p1[2]
       x2=p2[1]
       y2=p2[2]
     }else{
         x1=p2[1]
         y1=p2[2]
         x2=p1[1]
         y2=p1[2]
     }
    dX = x2 - x1
    dY = y2 - y1
    d12 = Eudist(p1,p2)
    if (len <=0){
        d34 = d12 *sqrt(3)
    }else{
        d34 = len/2
    }
    c = sqrt( (d12/2)^2 + (d34/2)^2) 
    alpha = atan( (y2-y1)/(x2-x1) )
        px=x1
        py=y1
    beta  = atan( d34 / d12) 
    theta = alpha + beta
xx=c(360/(2*pi/alpha),    360/(2*pi/beta),    360/(2*pi/theta) )
dx = c*cos(theta)
dy = c*sin(theta)
p3 = c(px + dx, py+dy)

    theta =2*pi - (beta - alpha )
xx=c(360/(2*pi/alpha),    360/(2*pi/beta),    360/(2*pi/theta) )
dx = c*cos(theta)
dy = c*sin(theta)
p4 = c(px + dx, py+dy)

    g1=rbind(p1,p2)
    g2=rbind(p3,p4)
#    ylim=range(p1,p2,p3,p4)
#    plot(g1[,1],g1[,2],type='l',col='red', asp=1, ylim=ylim,xlim=ylim)
#    lines(g2[,1],g2[,2], col='blue')
#    grid()
    return(list('p1'=p3, 'p2'=p4) )
}


tempttt <-function(mp1,mp2, len=0 ){
#mp1=cbind(runif(n=10, min=10,max=50),runif(n=10, min=-10,max=20))
#mp2=cbind(runif(n=10, min=10,max=50),runif(n=10, min=-10,max=20))
n=length(mp1)/2

mp3=mp1*0
mp4=mp1*0
for (i in 1:n){
    x=perpendicular(mp1[i,],mp2[i,],len)
    mp3[i,]=x$p1
    mp4[i,]=x$p2
}
g1=rbind(mp1,mp2);
g2=rbind(mp3,mp4)
ylim=range(c(g1,g2))
return(cbind(g1,g2))
    grid()

}
rep.row<-function(x,n){
    ret = x;
    for (i in 2:n){
        ret = rbind(ret,x)
    }
    return(ret)
}
rep.col<-function(x,n){
    ret = x;
    for (i in 2:n){
        ret = cbind(ret,x)
    }
    return(ret)
}


