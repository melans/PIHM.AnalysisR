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

ScnCompare <- function(Qsets, mcalib){
   
keyvar=apply(abs(mcalib-apply(mcalib,1,mean)),1,var)
keyid=which(keyvar!=0)
keyname=rownames(mcalib)[keyid]
uniqueval=apply(apply(mcalib[keyid,],1,unique),2,sort)

matvalue=expand.grid(uniqueval[,1],uniqueval[,2])
keepid=which(round(matvalue[,2])==matvalue[,2]);

    qm=apply.monthly(Qsets[,keepid],FUN=mean)
    #qm=Qsets[,keepid];
   qfdc=fdc(qm,plot=FALSE)
   
ord=apply(qfdc,2,order)
n1=nrow(uniqueval)
n2=n1;

n=ncol(ord)
tscolors=terrain.colors(n1)
tslty=1:n2;
matprop = expand.grid(1:n1,1:n2)
i=1;
ylim=range(qm,na.rm=TRUE)
plot(sort(qfdc[,i]), sort(as.numeric(qm[,i]),decreasing=TRUE) ,
       lty=matprop[i,2],col=tscolors[matprop[i,1]],log='y', type='l',
       xlab='Frequency',ylab='Discharge',ylim=ylim)

for (i in 2:n){  
  lines(sort(qfdc[,i]), sort(as.numeric(qm[,i]),decreasing=TRUE) ,
       lty=matprop[i,2],col=tscolors[matprop[i,1]])
}
#legend('topright',paste(keyname[1] ,'=',uniqueval[,1],sep=''),ncol=2,col=tscolors,lty=1)
#legend('bottomright',paste(keyname[2] ,'=',uniqueval[,2],sep=''),lty=1:n2)

legend('topright',c(paste(keyname[1] ,'=',(uniqueval[,1]-1)*100,'%',sep=''),
                    paste(keyname[2] ,'=',uniqueval[,2],sep='')),
       ncol=2,
       col=c(tscolors, rep(1,n2)),
       lty=c(rep(1,n1),1:n2) )


grid()



}
