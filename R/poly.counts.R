#Original code by Chris Brunsdon
#Edited by Binbin Lu
#Rmove all the functions from rgeos

poly.counts<-function (pts, polys)
{
  #colSums(gContains(polys, pts, byid=TRUE))
  if(is(pts, "Spatial"))
  {
    pts <- st_as_sf(pts)
    polys <- st_as_sf(polys)
  }
  rowSums(matrix(as.numeric(st_contains(polys, pts,sparse=F)),nrow=nrow(polys)))
}

  

poly.areas <- function(polys) 
{
  #gArea(polys, byid=TRUE)
  if(is(polys, "Spatial"))
  {
    polys <- st_as_sf(polys)
  }
  st_area(polys)
}
  

generalize.polys <- function(sfo,preserveTopology, dTolerance) 
{
  #gSimplify(sp,tol)
  if(is(sfo, "Spatial"))
  {
    sfo <- st_as_sf(sfo)
  }
  st_simplify(sfo, preserveTopology = preserveTopology, dTolerance = dTolerance)
}



