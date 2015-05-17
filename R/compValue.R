#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#' =============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
#' =============================================
#' @param  Path of output folder.
#' @keywords read output
#' @export  output data.
#' @examples
#' PIHM()
 
comValue <- function (folder,pattern,ext,id=0, obs){

    folder ='./output/';
#    pattern='lc.1505*';
    obs <- readUSGSgageLocal(fn='/Users/leleshu/Dropbox/octave/data/01576754-1985-2010.txt');

    projlist <- dir( path = folder, pattern = pattern,full.names=TRUE);
    projdir <- dir( path = folder, pattern = pattern,full.names=FALSE);
    outdir <- file.path(projlist,outdirname);
    dump<- lapply(outdir,function(x) if(!file.exists(x)) dir.create(path= x,showWarnings=FALSE,mode='0777') );
    pngfile <- file.path(outdir,paste(ext,'_',projdir,'.png',sep=''));
        


    Vlist <- list();
    np <- length(projlist)
    for (i in 1:np ) {
        VV <- readout(ext,path=projlist[i]);
        if (grepl('^GW',ext)){
            mesh <- readmesh();
            VV=VV[,1:mesh$size[1]];
        }
        if ( length(id) <= 1){
            if (id==0){
                V <- lapply(VV,mean);
            }
            else{
                V <- VV[,id];
            }
        }else{
             V <- lapply(VV[,id],mean);
        }       
        Vlist[[i]] <- V;
        png(file=pngfile[i],bg='transparent')    
        tsRainbow <- rainbow(ncol(VV))

        plot.zoo(x = VV, ylab = ext, main = paste(ext),
        col = tsRainbow , screens = 1)
        #line(obs,color='red',lwd=2);
        # Set a legend in the upper left hand corner to match color to return series
        #legend(x = "topright", legend = yname, lty = 1,col = tsRainbow)
        dev.off();

    }
    yname <- character(length(projdir));
    for(i in 1:length(projdir) ){
        yname[i] <- substr(projdir[i],6,nchar(projdir[i]) );
    }
    names(Vlist) <- yname;


    Vsets <- as.zoo(do.call(cbind,Vlist));
    tsRainbow <- rainbow(ncol(Vsets))
  
# Plot the overlayed series
outdir <- file.path(folder,'ScnComparison/');
dir.create(outdir,showWarnings=FALSE,mode='0777');
pngfile <- file.path(outdir,paste(ext,'_',substr(pattern,1,nchar(pattern)-1),'.png',sep=''))
png(file=pngfile,bg='transparent')    
plot(x = Vsets, ylab = ext, main = paste(ext,"comparison #",as.character(np)),
        col = tsRainbow, screens = 1)
#line(obs,color='red',lwd=2);
# Set a legend in the upper left hand corner to match color to return series
legend(x = "topright", legend = yname, lty = 1,col = tsRainbow)
dev.off();
  return(Vsets);
} 
comQ <- function (folder,pattern,obs){

#    folder ='./output/';
#    pattern='lc.1505*';
    obs <- readUSGSgageLocal(fn='/Users/leleshu/Dropbox/octave/data/01576754-1985-2010.txt');

    riv <- readriv();
    id <- riv$River$outlets;
    projlist <- dir( path = folder, pattern = pattern,full.names=TRUE);
    projdir <- dir( path = folder, pattern = pattern,full.names=FALSE);
    Qlist <- list();
    np <- length(projlist)
    for (i in 1:np ) {
        qq <- readout('rivFlx1',path=projlist[i]);
        Q<- qq[,id];
        Qlist[[i]] <- Q;
    }
    yname <- character(length(projdir));
    for(i in 1:length(projdir) ){
        yname[i] <- substr(projdir[i],6,nchar(projdir[i]) );
    }
    names(Qlist) <- yname;


    Qsets <- as.zoo(do.call(cbind,Qlist));
    tsRainbow <- rainbow(ncol(Qsets))
  
# Plot the overlayed series
plot(x = Qsets, ylab = "Discharge", main = paste("Discharge comparison #",as.character(np),sep=''),
        col = tsRainbow, screens = 1)
#line(obs,color='red',lwd=2);
# Set a legend in the upper left hand corner to match color to return series
legend(x = "topright", legend = yname, lty = 1,col = tsRainbow)


  return(Qsets);
}
