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


datafilter <-function(data, filter,name='value',ylab='Value Name', unit='',is.riv=FALSE,if.plot=TRUE){
    dataMin=sapply(data,min);
    dataMax=sapply(data,max);

    if(is.riv){ #filter for riv
        key='OverBanks'
        riv =readriv();
        segshp=riv$River$riv[,7]
        shp=riv$Shape$shp;
        calib=readcalib(bak=TRUE);
        rd <- shp[segshp,2] * calib$value['RIV_DPTH'];
        
        ids=which(dataMax>rd);
        cat("\t",as.character(length(ids)), "item(s) are filtered from ",name," data.", "filter= Over Banks\n\n");
    }else{
        if(missing(filter)){    #calculate default filter.
            sd=soildepth();
            filter=sd;
        }
        if (length(filter)==ncol(data)){ #filter value for each cell.
            if(mean(filter)>0){
                ids=which(filter<dataMax); 
            }else{
                ids=which(filter<dataMin);
            }
            key='Vfilter'
            cat("\t",as.character(length(ids)), "item(s) are filtered from ",name," data.", 'filter= vector',"\n\n");
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
            key=as.character(mean(filter) );
        }
    }
    
    plotzoo(data,fn=paste(name,'_all.png',sep=''),ylab=ylab,unit=unit) ;
    
    if(length(ids)==0){
        return(0)
    }else{
        if(if.plot){
            plotzoo(data[,ids],fn=paste(name,key,'.png',sep='') ,ylab=ylab,unit=unit,holdon=TRUE);
            if (is.riv){
                urd=sort(unique(rd))
                xx=range(time(data));
                for (i in 1:length(urd)){
                    lines(xx,c(urd[i],urd[i]),lwd=2);
                }
                dev.off();
            }
            if(is.riv){ # if ids is for rivers.
                PIHM.triplot(rivid=ids,fn=paste(name,'_Overbank.png',sep=''),name=name,title=paste(name) );
            }else{  # ids is for Cells.
                PIHM.triplot(cellid=ids,fn=paste(name,'+ELV',key,'.png',sep=''),name=name,title=paste(name,'filter=',key) );
            }
        }
    }
   pihm.dev.close() 
    return(ids)
}


datafilter.riv <-function(data, filter,name='rivvalue',ylab='riv Value Name', unit='',if.plot=TRUE){
    dataMin=sapply(data,min);
    dataMax=sapply(data,max);

    if(missing(filter)){ #default river filter is "OVER BANKS"
        key='OverBanks'
        riv =readriv();
        segshp=riv$River$riv[,7]
        shp=riv$Shape$shp;
        calib=readcalib(bak=TRUE);
        rd <- shp[segshp,2] * calib$value['RIV_DPTH'];
        
        ids=which(dataMax>rd);
        cat("\t",as.character(length(ids)), "river segment(s) are filtered from ",name," data.", "filter= Over Banks\n\n");
    }else{
        if (length(filter)==ncol(data)){ #filter value for each cell.
            if(mean(filter)>0){
                ids=which(filter<dataMax); 
            }else{
                ids=which(filter<dataMin);
            }
            key='Vfilter'
            cat("\t",as.character(length(ids)), "river segment(s) are filtered from ",name," data.", 'filter= vector',"\n\n");
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
            cat("\t",as.character(length(ids)), "river segment(s) are filtered from ",name," data.", 'filter=',filter,"\n\n");
            key=as.character(mean(filter) );
        }
    }
    
    plotzoo(data,fn=paste(name,'_all.png',sep=''),ylab=ylab,unit=unit) ;
    
    if(length(ids)==0){
        return(0)
    }else{
        if(if.plot){
            plotzoo(data[,ids],fn=paste(name,key,'.png',sep='') ,ylab=ylab,unit=unit,holdon=TRUE);
            if (missing(filter)){
                urd=sort(unique(rd))
                xx=range(time(data));
                for (i in 1:length(urd)){
                    lines(xx,c(urd[i],urd[i]),lwd=2);
                }
            }
            dev.off();
            PIHM.triplot(rivid=ids,fn=paste(name,'_',key,'.png',sep=''),name=name,title=paste(name) );
         }
    }
   pihm.dev.close() 
    return(ids)
}
