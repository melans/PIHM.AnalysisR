#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' @param  fn Full path of input file. The file has to be in matrix format and first colomn is in "YYYY-MM-DD hh:mm".
#' @keywords read output. Could be used for reading mesh and river format.
#' @export  output data.
#' @return A TimeSeries data. This list require support of xts packages.
#' @examples
#' readout(ext='rivFlx1',binary=FALSE)

LaiRf <- function(lc,years, if.daily=FALSE){
    #years=2000:(2010+1);
    years=sort(c(years,max(years)+1))
    yrlim=range(years);
    ny = length(years)
    t1=as.Date(paste(yrlim[1],'-01-01',sep=''))
    t2=as.Date(paste(yrlim[2],'-12-31',sep=''))
    tdaily = seq.Date(t1,t2,by=1)
    DataDaily=as.xts(numeric(length(tdaily)),order.by=tdaily)
    DataMon=apply.monthly(DataDaily,FUN=sum)
    tmon = time(DataMon)- days_in_month(time(DataMon))+1
    nlc=length(lc)
    l = matrix(0, nrow=12, ncol=nlc)
    r = matrix(0, nrow=12, ncol=nlc)
    for (i in 1:nlc){
        l[,i] = cfun(lc[i], type=1)
        r[,i] = cfun(lc[i], type=2)
    }
    lmat = as.xts(rep.row(l, ny), order.by=tmon)
    rmat = as.xts(rep.row(r, ny), order.by=tmon)
    colnames(lmat)=lc
    colnames(rmat)=lc
    ret=list('LAI'=lmat, 'Rough'=rmat)
    if(if.daily){
        ld = NA*rep.col(DataDaily, nlc);
        rd = NA*rep.col(DataDaily, nlc);
        ld[time(lmat),]=lmat
        rd[time(rmat),]=rmat
        ld=na.approx(ld)
        rd=na.approx(ld)
        colnames(ld)=lc
        colnames(rd)=lc
        ret=list('LAI'=ld, 'Rough'=rd)
    }
    return(ret)
}


MeltFactor <- function(years){
    mf=c(0.001308019, 0.001633298,  0.002131198, 0.002632776, 0.003031171,  0.003197325, 0.003095839, 0.00274524,     0.002260213, 0.001759481, 0.001373646,  0.001202083); 
    years=sort(c(years,max(years)+1))
    yrlim=range(years);
    ny = length(years)
    t1=as.Date(paste(yrlim[1],'-01-01',sep=''))
    t2=as.Date(paste(yrlim[2],'-12-31',sep=''))
    tdaily = seq.Date(t1,t2,by=1)
    DataDaily=as.xts(numeric(length(tdaily)),order.by=tdaily)
    DataMon=apply.monthly(DataDaily,FUN=sum)
    tmon = time(DataMon)- days_in_month(time(DataMon))+1
    nlc=length(lc)
    ret = as.xts(rep(mf, ny), order.by=tmon)
    return(ret)
}
