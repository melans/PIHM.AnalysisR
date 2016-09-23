###########################################################################################
###########################################################################################
readnwis <- function(url){
    if(missing(url)){
        url = 'http://nwis.waterdata.usgs.gov/nwis/peak?site_no=01576500&agency_cd=USGS&format=rdb'
        message('\nDemo:')
        message('URL = \n\t', url)
    }
    
    utext=getWebServiceData(url) 
   ltext=as.matrix(read.csv(text=utext[1],  skipNul=TRUE, sep='\n') )
   id=which(grepl('#', ltext))
   meta=ltext[id,];
   tab=as.matrix(read.csv(text=ltext[-id,], header=TRUE, sep='\t'))
    ret =list('data'=tab[-1,], 'meta'=meta, 'FieldInfo'=tab[-1,])
    return(ret)
}


###########################################################################################
###########################################################################################
readnwisDaily.GWL <- function(siteid, StartDate=Sys.Date()-100*365, EndDate=Sys.Date(), SI.unit=TRUE){
weburl = 'http://waterdata.usgs.gov/nwis/dv?'

#cb_all_72019 = 'on' ;
#cb_72019 = 'on' ;
format = 'rdb' ;
#site_no = '401148076105901' ;
site_no = paste(siteid) ;
#referred_module = 'sw' ;
period = '' ;
#begin_date =  '1986-05-09' ;
#end_date = '1986-09-30' ;
begin_date = paste(StartDate) ;
end_date = paste(EndDate);
if (SI.unit){
    uf = 0.3048 ;   # ft to meter
}else{
    uf =1
}

url = paste(weburl,
#         paste0( 'cb_all_72019'    ,'=', cb_all_72019),
#         paste0( 'cb_72019'        ,'=', cb_72019),
         paste0( 'format'          ,'=', format),
         paste0( 'site_no'         ,'=', site_no),
#         paste0( 'referred_module' ,'=', referred_module),
         paste0( 'period'          ,'=', period),
         paste0( 'begin_date'      ,'=', begin_date),
         paste0( 'end_date'        ,'=', end_date)
         , sep='&')
tmpdata=getWebServiceData(url)
tab=read.table(text=tmpdata, sep='\t',skipNul=TRUE,skip=24, header=TRUE)
tab=tab[-1,]
#head(tab)
 t=as.Date(tab[,3], tz='UTC')
data = as.xts(as.numeric(as.matrix((tab[,4]))) * uf, order.by=t)
 return (data)
}

