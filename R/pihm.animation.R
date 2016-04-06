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

PIHM.AnimationMap<-function(data,range=0,dt=0,terrain=FALSE,name='value',zratio=0,dist=0,fn='2Dmap.png',path=Resultpath,title='map',colorFUN=terrain.colors,
                     Hlim, colorreverse=FALSE , ngrids=200,riveron=TRUE,
                     addcontour=FALSE, D3D=FALSE){

    if(range[1] ==0){
        nt = nrow(data)
    }else{
        nt=length(range)
    }
    if(dt==0){
        dt=round(nt/10)
        if(dt <=0) dt=1;
    }
    ts = seq(from = 1, to=nt, by =dt) 
    tstime= time(data)
    Hlim=range(data,na.rm=TRUE)
    if (terrain){
        mesh <- readmesh(bak=TRUE);
        riv <- readriv(bak=TRUE);
        msh <- mesh$mesh;
        pts <- mesh$points;
        z=(pts[msh[,2],4]+pts[msh[,3],4]+pts[msh[,4],4])/3;
        Hlim = Hlim + range(z,na.rm=TRUE)
    }
    for (i in ts){
        iname = paste(name,tstime[i]);
        ifn= paste(name,tstime[i], '.png',sep='')
        message('file=',ifn);
        if(D3D){
            PIHM.3Dmap(data[i,],terrain=terrain,name=iname,dist=dist,fn=ifn,path=path,title=title,colorFUN=colorFUN,
                           Hlim=Hlim, colorreverse=colorreverse ,
                           ngrids=ngrids,riveron=riveron);

        }else{
            PIHM.mapraster(data[i,],terrain=terrain,name=iname,dist=dist,fn=ifn,path=path,title=title,colorFUN=colorFUN,
                       Hlim=Hlim, colorreverse=colorreverse ,
                       ngrids=ngrids,riveron=riveron,addcontour=addcontour);
        }
    }
        
}
