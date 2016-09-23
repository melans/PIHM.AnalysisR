#############################################################
# Notes
#
#
# See Notes for packages needed
#############################################################
getSoilData <- function(MUKEY, vars = c('silttotal_r', 'sandtotal_r', 'om_r',
                                        'dbthirdbar_r', 'chkey','hzname'),
                         fn = 'gSSURGO_texture.csv', na.rm=TRUE){
    
loadinglib('soilDB')
in.statement <- format_SQL_in_statement(MUKEY)

#in.statement = paste(MUKEY);
q <- paste("SELECT component.mukey, ",paste( paste0('chorizon.', vars), collapse=',') ,
           "FROM component ", 
           "JOIN chorizon " , 
           "ON component.cokey = chorizon.cokey AND mukey ",
           "IN ", in.statement, 
           "ORDER BY mukey ", sep=" ")
dat <- SDA_query(q)
dat <- as.data.frame(dat)
ret = dat
#head(dat)
#write.csv(dat, file=fn)
#
#if(na.rm){
#    mn = which(is.na(dat), arr.ind=TRUE)
#    id = mn[,1]
#    ret = dat[-id,];
#}else{
#    ret=dat;
#}

write.csv(ret, file=fn)

return(ret)
}
