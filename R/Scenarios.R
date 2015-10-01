#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#'  <- ============================================
#'  Current version is for PIHM 2.4 and above;



AllScenarios <- function (inpath=inpath,folder,pattern,ext,id=0, obs, projdir){
    pihm.dev.close()
    PIHM.3Dclose();
    folder ='./';
#    pattern='lc.150617*';
    if (missing(obs)){
        siteid=siteid();
        if (!grepl('^unk', siteid)){
            obs <- readUSGSQ(siteid=siteid());
        }
    }

    if (missing(projdir)){     
        projlist <- dir( path = folder, pattern = pattern,full.names=TRUE);
        projdir <- dir( path = folder, pattern = pattern,full.names=FALSE);
    }else{
        for(i in 1:length(projdir)){
            nc=nchar(projdir[i]);
            if (grepl('/$',projdir[i])){                
                projdir[i]=substr(projdir[i],1,nc-1)
            }
        }
        projlist <- file.path(folder, projdir);
    }
    nsets=length(projlist);

    scn=list(nsets)
    for(i in 1:nsets)       
    {  
        message(i, "/", nsets, "\t", projlist[i] , "\n");
        null=PIHM.path(indir=inpath, outdir=projlist[i],pname=projectname)
        out= BasicAna(quiet=TRUE, obsQ=obs, reload=TRUE);
        message('\t\t #', i, ' is finished. \t@',Sys.time(),'\n' );
        scn[[i]]=out;
    }
    names(scn)=projdir;
   pihm.dev.close()
   PIHM.3Dclose();
   return(scn)
}

