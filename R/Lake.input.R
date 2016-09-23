#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Mon Sep 19 16:10:28 EDT 2016
#'  <- ============================================
#'  Current version is for PIHM 2.0 and above;

lakeBndOrder <- function (x,xy, nodes ){
    x=vt.lake;  # vertex of lake boundary.
    xy = pt.lakebd[,1:2]
    ct = colMeans(xy)
    nodes = segs
    nx = length(x)
    nd = nodes;
    key = x[1]; #current pivot points
    ord = numeric(nx+1)
    ord[1] = key;
    for (ix in 1:nx){
                print(nd)
        pos = which(nd == key, arr.ind=TRUE)
            message('i =', ix, '\tkey=',key, '\trow=', pos[1]);
        row = nodes[pos[1], ]
        row[row ==key]=NA
        poss  = which(row %in% x);
        if(length(poss) ==1){
            nxt = row[poss]
        }else{
            nxt = row[poss]
        }        
        ord[ix+1] = nxt
        key = nxt
        nd=nd[-pos[1],]
   }
    return(ord)
}


lake.boundnode <- function(path=outpath,
            file=file.path(path, paste0(projectname,'.lakeboundnode') ) ){
    if(!file.exists(file)){
        stop('File does not exist', file)
    }

att=lake.readatt(bak=bak)
bathy=lake.readbathy(bak=bak)
geo = lake.readgeom(bak=bak);
soil = lake.readsoil(bak=bak)
    tline= readLines(file, skipNul=TRUE)
    geo = lake.readgeom();
    len =as.numeric(tline[length(tline)+1-rev(1:nrow(att))])
    
   # len=unlist(lapply(geo$banklist,length))
    pgs = list();
    for( i in 1:length(len)){
        id=1:len[i];
        x=as.matrix(read.table(text=tline[id], header=FALSE))
        coords=rbind(x,x[1,])
         ipg = Polygon(cbind(coords))
        pgs[[i]] = Polygons(list(ipg), i)
        tline=tline[-id]
    }
    dbf = cbind('att'=att, 'bathy'=bathy, 'geo'=geo$info, 'soil'=soil)
    colnames(dbf)=c(paste0('att.', colnames(att)), 
                paste0('bathy.', colnames(bathy)), 
                paste0('geo.', colnames(geo$info)),
                paste0('soil.', colnames(soil) )
                       )

    pog = SpatialPolygons(pgs)
    ret = SpatialPolygonsDataFrame(Sr=pog, data=as.data.frame(dbf),
                                   match.ID=colnames(dbf)[1])
    gispath=file.path(ModelInfopath,'GISlayer')
    writeSpatialShape(ret,fn=file.path(gispath,'Lakes'))
return(ret)


}
lake.readatt <-function(bak=TRUE, theFile=getFilePath('lakeatt')){
    lines <- readLines(theFile, skipNul=TRUE);
    atthead =c('INDEX', 'SOIL', 'METEO','MACP')
        matatt <- as.matrix(read.table(text = lines) )
    colnames(matatt)=toupper(atthead);
    return(matatt);
}


lake.readbathy<-function(bak=TRUE, theFile=getFilePath('lakebathy')){
        
    lines <- readLines(theFile, skipNul=TRUE);
    atthead =toupper(c('SURF', 'LAKEBED','BEDROCK') )
        matatt <- as.matrix(read.table(text = lines, row.names=1) )
    colnames(matatt)=toupper(atthead);
    return(matatt);
}

lake.readgeom<-function(bak=TRUE, theFile=getFilePath('lakegeom')){
    lines <- toupper(readLines(theFile, skipNul=TRUE) )
    lid = which(grepl('^LAKE', lines) ) 
    lakeatt <- as.matrix(read.table(text = lines[lid], header=TRUE, row.names=1) )

    lid = which(grepl('^BANK', lines) ) 
    blist = list();
    for (i in 1:length(lid)){
        bid <- as.numeric(read.table(text = lines[lid[i]], header=FALSE,
                                        row.names=1) )
        blist[[i]]= bid;
    }
    names(blist)= rownames(lakeatt);
    ret = list ('info'=lakeatt,
                'banklist'=blist)
    return(ret);
}


lake.readsoil<-function(bak=TRUE, theFile=getFilePath('lakesoil')){
    lines <- readLines(theFile, skipNul=TRUE);
    ns = as.numeric(unlist(read.table(text=lines[1])))
    vhead =toupper(c('Kh_matrix','Kv_matrix','ThetaS','ThetaR','macKsatH') )
    mat <- as.matrix(read.table(text = lines[-1], row.names=1) )
    colnames(mat)=toupper(vhead);
    return(mat);
}
