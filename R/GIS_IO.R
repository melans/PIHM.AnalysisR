

Mesh2Shp <- function( att=readatt(), mesh=readmesh(shp=FALSE),path=file.path(ModelInfopath,'GISlayer'))
{
    ncell = nrow(mesh$mesh)
    dir.create(path, recursive=TRUE, showWarnings =FALSE );
    #X,Y value
    p.x = mesh$points[,2]
    p.y = mesh$points[,3]
    ipt = t (mesh$mesh[1:ncell,c(2:4,2)] )
    x = matrix( p.x[ipt], nrow=4)
    y = matrix( p.y[ipt], nrow=4)
    
    #Z value
    p.zmin = mesh$points[,4]
    p.zmax = mesh$points[,5]
    ct.pt = t (mesh$mesh[1:ncell,c(2:4)] ) 
    ct.x = matrix( p.x[ct.pt], nrow=3)
    ct.y = matrix( p.y[ct.pt], nrow=3)
    ct.zmin = matrix( p.zmin[ct.pt], nrow=3)
    ct.zmax = matrix( p.zmax[ct.pt], nrow=3)
   
    c.x = colMeans(ct.x)
    c.y = colMeans(ct.y)
    c.min = colMeans(ct.zmin)
    c.max = colMeans(ct.zmax)
    
    #att
    dbf = cbind('X.c'=c.x, 'Y.c'=c.y,  'Zmin'=c.min, 'Zmax'= c.max, att )
    dbf = as.data.frame(dbf)
     # shapefile of centroid, point, 
    sp_points = SpatialPointsDataFrame(coords=cbind(c.x,c.y),
                                       data=as.data.frame(dbf))
    writeSpatialShape(sp_points,fn=file.path(path,'Cell_center'))
    # shapefile of mesh polygons.
    pgs = list()
    for (i in 1:ncell){
        ipg = Polygon(cbind(x[,i], y[,i]))
        pgs[[i]] = Polygons(list(ipg), i)
    }
    pog = SpatialPolygons(pgs)
    rownames(dbf) = names(pog)
    ret = SpatialPolygonsDataFrame(Sr=pog, data=as.data.frame(dbf))
    writeSpatialShape(ret,fn=file.path(path,'Cells'))
    return(ret)
}
