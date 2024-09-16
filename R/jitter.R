#Original code by Chris Brunsdon
#Edited by Binbin Lu

jitter.points <- function(pts,scl) {
  	spdf <- TRUE
	if(inherits(pts, "Spatial"))
	  {
		x <- coordinates(pts)
		x <-  x + rnorm(length(x),0,scl) 
		res <- SpatialPoints(x)
		proj4string(res) <- CRS(proj4string(pts))
		spdf <- TRUE 
  	}   
  	else
  	{
		x <- st_coordinates(pts)
		x <-  x + rnorm(length(x),0,scl) 
		res <- x
  	}
	if (spdf) {
		res <- SpatialPointsDataFrame(res,data.frame(pts))}
	else
	{
		pts$geom <- res
		res <- pts
	}
	return(res) 
}

bstrap.points <- function(pts) {
  if(inherits(pts, "Spatial"))
    x = coordinates(pts)
  else
    x = st_coordinates(pts)
	x = x[sample(nrow(x),replace=TRUE),]
	if (inherits(pts, "SpatialPointsDataFrame")) {
		res = SpatialPoints(x)
		proj4string(res)=CRS(proj4string(pts))
		res = SpatialPointsDataFrame(res,data.frame(pts))}
	else
	{
		pts$geom <- st_coordinates(pts)
		res <- pts
	}
	return(res) }
	
poly.labels <- function(polys) {
	pts = SpatialPoints(t(sapply(slot(polys,'polygons'), function(x) slot(x,'labpt'))))
	proj4string(pts) = CRS(proj4string(polys))
	pts }

