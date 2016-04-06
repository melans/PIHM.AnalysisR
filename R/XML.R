#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by  Thu Apr 16 09:49:53 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;


xml.gssurgo2csv <- function(path='./', fn='HT_GSSURGO_Variables.xml',file){
    if(missing(file)){
        file=file.path(path,fn)
    }
    loadinglib(liblist='XML')
    xf <- xmlTreeParse(file)
    xlist=xmlToList(xf)
    nsoil = as.numeric(xlist$Soil_Output$Soil_Count_List$Total_Count)
    soillist=xlist$Soil_Output$Soil_List
    data=list()
    varnames=c('id', 'mukey', 'silttotal_r', 'claytotal_r', 'sandtotal_r','om_r','ksat_r','dbthirdbar_r','partdensity')
    nVar=length(varnames);
    mat= matrix(NA, nsoil, nVar)
    colnames(mat)=varnames;
    #for (i in 1:nsoil){
    mat[,1]=1:nsoil;
    for (i in 1:nsoil){
        s = soillist[[i]]
        sv = matrix(unlist(s))
        vnames= names(s)
        rownames(sv)=vnames
        data[[i]]=sv;
        for(j in 2:nVar){
            ids=which(grepl(varnames[j], vnames));
            if(length(ids)>0){
                mat[i,j]=as.numeric(sv[ids[1]])
            }
            s[ids]
            head(mat)
        }
    }
    
    rmat=unique(mat[,2:nVar]);
    rmat=cbind(1:nrow(rmat), rmat);
    colnames(rmat)=varnames;
    
    csvfile=paste(file,'.csv',sep='');
    write.csv(file=csvfile, x=rmat,quote=FALSE, row.names=FALSE)

    return(rmat)
}


