#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;

Eudist<-function(pt1,pt2){
    x1=pt1[,1];
    x2=pt2[,1];
    y1=pt1[,2];
    y2=pt2[,2];
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
