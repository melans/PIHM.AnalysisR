#' PIHM Analysis project.
#' Developed by Lele Shu( lele.shu at gmail.com  lzs157 at psu.edu ) 
#' Created by Wed Apr 15 20:25:45 EDT 2015
#' =============================================
#'  Current version is for PIHM-MF or PIHM v2.4;
#' 
#' 
removechanngel <- function(){
riv=readriv()
msh=readmesh();
cmesh=msh$mesh

mriv=riv$River$riv
riv$River$outlets
pt.to=mriv[riv$River$outlets, 'TO']    
cellid=which(cmesh[,2]==pt.to | cmesh[,3]==pt.to |cmesh[,4]==pt.to) #possible cells.
PIHM.triplot(cellid=cellid)



}
