#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 

minfo <- function(bak,mapplot=PIHM.triplot,if.plot=TRUE){
    path=ModelInfopath;
    theFile <- file.path(path, paste(projectname,".",'minfo.txt',sep=''));
    mesh=readmesh(bak=bak);
    riv=readriv(bak=bak);
    K=suppressWarnings(Kcalib(calib.bak=bak,path=ModelInfopath,quiet=TRUE) );
    soil=readsoil(bak=FALSE);
    geol=readgeol(bak=FALSE);
    lc=readlc(bak=bak);
    att=readatt(bak=bak);
   
    nm=mesh$size[1]
    np=mesh$size[2]
    nr=riv$River$size;
    no=length(riv$River$outlets);

    nsoil=nrow(soil);
    usoil=sort(unique(att[,2]));
    unsoil=length(usoil);

    ngeol=nrow(geol);
    ugeol=sort(unique(att[,3]));
    ungeol=length(ugeol)

    nlc=nrow(lc);
    ulc=sort(unique(att[,4]));
    unlc=length(ulc);

        
    para<-readpara(bak=bak);    
    start<-para[[which(grepl('^start',tolower(names(para))))]];
    end<-para[[which(grepl('^end',tolower(names(para))))]];
    dt <- para[[which(grepl('^rivflx1',tolower(names(para))))]];


    calib<-readcalib(bak=bak);
    
    write(paste('Name of Project = \t',projectname),file=theFile,append=FALSE);
    write(paste('From\t',start,'To\t',end ),file=theFile,append=TRUE);
    write('',file=theFile,append=TRUE);


    write(paste('Size of Mesh = \t',nm),file=theFile,append=TRUE);
    write(paste('Size of points = \t',np),file=theFile,append=TRUE);
    write(paste('Size of river = \t',nr),file=theFile,append=TRUE);
    write(paste('Size of outlets = \t',no),file=theFile,append=TRUE);
    write('',file=theFile,append=TRUE);

    write(paste('Size of soil = \t',unsoil,"/",nsoil),file=theFile,append=TRUE);
    write(paste('Size of geol = \t',ungeol,"/",ngeol),file=theFile,append=TRUE); 
    write(paste('Size of LC = \t',unlc,"/",nlc),file=theFile,append=TRUE);
    
        write('',file=theFile,append=TRUE);
    write(paste('Unique of soil = \t',paste(usoil,collapse=' ')),file=theFile,append=TRUE);
    write(paste('Unique of geol = \t',paste(ugeol,collapse=' ')),file=theFile,append=TRUE);
    write(paste('Unique of LC = \t',paste(ulc,collapse=' ')),file=theFile,append=TRUE);
    
        write('\nm/day',file=theFile,append=TRUE);
    write(paste(colnames(K$cKsoil),collapse=' '),file=theFile,append=TRUE);
    suppressWarnings(write(K$cKsoil[nrow(K$cKsoil),],file=theFile,append=TRUE))

        write('\nm/day',file=theFile,append=TRUE);
    write(paste(colnames(K$cKgeol),collapse=' '),file=theFile,append=TRUE);
    suppressWarnings(write(K$cKgeol[nrow(K$cKgeol),],file=theFile,append=TRUE))

        write('\nm/day',file=theFile,append=TRUE);
    suppressWarnings(write.table(K$Krivm,file=theFile,row.names=TRUE,col.names=TRUE,,quote=FALSE,append=TRUE))


    
 if(if.plot){
    PIHM.triplot(data=att[,2],fn=paste(projectname,'_Soil_map.png',sep=''),name='Soil Type',title=paste('Soil Map of', projectname), path=path);
    PIHM.triplot(data=att[,3],fn=paste(projectname,'_Geol_map.png',sep=''),name='Geol Type',title=paste('Geology Map of', projectname), path=path);
    PIHM.triplot(data=att[,4],fn=paste(projectname,'_LC_map.png',sep=''),name='Land Cover Type',title=paste('Land Cover Map of', projectname), path=path);

    mapplot(data=soildepth(),fn=paste(projectname,'_Soil_Depth.png',sep=''),name='Soil depth(m)',title=paste('Soil depth of', projectname), path=path);
    
    mapplot(data=soil[att[,2],2]*86400 * calib$value['KINF'] ,fn=paste(projectname,'_Kinf.png',sep=''),name='K infilration(m/day)',title=paste('Conductivity of Infiltration', projectname), path=path);
    mapplot(data=geol[att[,3],2]*86400* calib$value['KSATH'] ,fn=paste(projectname,'_KH.png',sep=''),name='K Horizontal(m/day)',title=paste('Horizontal Conductivity of', projectname), path=path);
    mapplot(data=geol[att[,3],3]*86400* calib$value['KSATV'] ,fn=paste(projectname,'_KV.png',sep=''),name='K Vertical(m/day)',title=paste('Vertical Conductivity of', projectname), path=path);
    
    #VegFrac
    mapplot(data=lc[att[,4],2]* calib$value['VEGFRAC'] * 100 ,fn=paste(projectname,'_VegFrac.png',sep=''),name='VEGFRAC(%)',title=paste('Vegetation fraction of', projectname), path=path);
    #LAI max
    mapplot(data=lc[att[,4],9] ,fn=paste(projectname,'_LAI_max.png',sep=''),name='LAI max(m2/m2)',title=paste('Max LAI of', projectname), path=path);
    #Roughness
    mapplot(data=lc[att[,4],16] ,fn=paste(projectname,'_Roughness.png',sep=''),name='Roughness(m)',title=paste('Roughness of', projectname), path=path);

 }



    

    
}
#============ #============
#============ #============


landcovertable <-function(bak=TRUE,class=FALSE, output=ModelInfopath)
{
    lc=readlc(bak=bak);
    att=readatt(bak=bak);
   
    nlc=nrow(lc);
    ulc=sort(unique(att[,4]));
    unlc=length(ulc);

    iarea=readarea(bak=bak);
    area=sum(iarea)
    lca=matrix(0,ncol=2,nrow=unlc)
    for (i in 1:unlc){
        lca[i,1] = sum(iarea[which(att[,4]==ulc[i])]);
        lca[i,2] = 100 * lca[i,1]/area
    }
    lca=rbind(lca,colSums(lca))
    cname=c('Area(m2)', 'Percentage')
    colnames(lca)=cname
    rname=c(as.character(ulc), 'sum')
    rownames(lca)=rname
    
    theFile <- file.path(path=output, paste(projectname,".",'lctable.txt',sep=''));
    write.table(lca,file=theFile,append=FALSE);

    if (class){
        
        cl=matrix(0,ncol=2,nrow=5); 
        type=list(c(23,24), 
                  c(20,21,22),
                  c(40,41,42),
                  c(80,81,82)
                  )
        # 1-Urban 23 24;
        # 2-SubUrban 20 21 22
        # 3-Forest 40 41 42
        # 4-Agriculture 80 81 82
        # 5-Other  11 30x 90x
        i=1;
        id=which(ulc>=23 & ulc<30)
        cl[i,]=colSums(lca[id,]);
        ids=id;
        
        i=2;
        id=which(ulc>=20 & ulc<23)
        cl[i,]=colSums(lca[id,]);
        ids=c(ids,id)

        i=3;
        id=which(ulc>=40 & ulc<50)
        cl[i,]=colSums(lca[id,]);
        ids=c(ids,id)

        i=4;
        id=which(ulc>=80 & ulc<90)
        cl[i,]=colSums(lca[id,]);
        ids=c(ids,id)

        i=5;
        ss=1:unlc
        id=ss[!(ss %in% ids)]
        cl[i,]=colSums(lca[id,]);
        
        cl=rbind(cl,colSums(cl));

        rname=c('Urban','SubUrban', 'Forest', 'Agricultrue', 'Other','SUM')
        colnames(cl)=cname;
        rownames(cl)=rname;
        write.table(cl,file=theFile,append=TRUE);
        ret=cl;
    }else{
        ret = lca
    }

    return(ret);
    
}
