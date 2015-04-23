#' Find the cells/segments that match your filters.

#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 

#' Created by Fri Apr 17 14:11:46 EDT 2015
#' 
#' @param data in TS
#' @param filter. If filter<0, find the IDs where is smaller than filter.  If filter>0, find the IDs where is larger than filter.
#' @param ifplot. Defaut=FALSE
#' @keywords check value
#' @export  id of cell/segment which match filter.
#' @examples
#' datafilter(GW,filter=5) to find the cells which has value>5.
#'
#' datafilter(GW,filter=-5) to find the cells which has value<5.
#' 
#' datafilter(GW,filter=[-5,5]) to find the cells which has -5<value<5.
#' 
#' 
#' 


datafilter <-function(data, filter=5,ifplot=FALSE){
    dataMin=sapply(data,min);
    dataMax=sapply(data,max);
    filterMin=min(filter);
    filterMax=max(filter);
    if (length(filter)>1)    {
        ids=which(dataMin>filterMin & dataMax< filterMax) 
    }else{
        if (filter >0){
            ids=which(dataMax>filterMax);
        }else{
            ids=which(dataMin<filterMin);
        }
    }
    cat("\t",as.character(length(ids)), "item(s) are filtered\n\n");
    if(ifplot & length(ids)>0){
        plot.new();
        matplot(data[,ids],type='l');
    }
}
