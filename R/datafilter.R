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
#' datafilter(data,filter=5) to find the cells which has value>5.
#'
#' datafilter(data,filter=-5) to find the cells which has value<5.
#' 
#' datafilter(data,filter=[-5,5]) to find the cells which has -5<value<5.
#' 
#' 
#' 


datafilter <-function(data, filter=5,name='value',ylab='Value Name', unit='',is.riv=FALSE,if.plot=TRUE){
    dataMin=sapply(data,min);
    dataMax=sapply(data,max);
    if(is.riv){
        riv =readriv();
        segshp=riv$River$riv[,7]
        shp=riv$Shape$shp;
        rd <- shp[segshp,2];
        ids=which(dataMax>rd);
    cat("\t",as.character(length(ids)), "item(s) are filtered from ",name," data.", "filter= Over Banks\n\n");
    }else{
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
    cat("\t",as.character(length(ids)), "item(s) are filtered from ",name," data.", 'filter=',filter,"\n\n");
    }
    
    plotzoo(data,fn=paste(name,'_all.png',sep=''),ylab=ylab,unit=unit) ;
    
    if(length(ids)==0){
        return(0)
    }else{
        if(if.plot){
            plotzoo(data[,ids],fn=paste(name,filter,'.png',sep='') ,ylab=ylab,unit=unit);
            if(is.riv){ # if ids is for rivers.
                PIHM.triplot(rivid=ids,fn=paste(name,'_Overbank.png',sep=''),name=name,title=paste(name) );
            }else{  # ids is for Cells.
                PIHM.triplot(cellid=ids,fn=paste(name,'+ELV',filter,'.png',sep=''),name=name,title=paste(name,'filter=',filter) );
            }
        }
    }
    return(ids)
}
