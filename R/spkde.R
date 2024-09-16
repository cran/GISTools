# Kernel Densities from point object
#Original code by Chris Brunsdon
#Edited by Binbin Lu

kde.points <- function(pts, h, n = 200, lims = NULL) {
  spdf <- FALSE
  
  # Check if input is a Spatial object
  if (inherits(pts, "Spatial")) {
    xy <- coordinates(pts)
    p4s <- CRS(proj4string(pts))
    spdf <- TRUE
  } else {
    xy <- st_coordinates(pts)
    p4s <- CRS(st_crs(pts)$proj4string)
  }
  
  # Determine bandwidth if not provided
  if (missing(h)) h <- c(bandwidth.nrd(xy[, 1]), bandwidth.nrd(xy[, 2]))
  
  # Set limits for the grid
  if (is.null(lims)) {
    lims <- c(range(xy[, 1]), range(xy[, 2]))
  } else {
    if (inherits(lims, "Spatial")) {
      lims <- t(bbox(lims))
    } else {
      lims <- st_bbox(lims)[c(1, 3, 2, 4)]
    }
  }
  
  # Perform kernel density estimation
  kd <- kde2d(xy[, 1], xy[, 2], h, n, lims)
  
  #if (spdf) {
    # Create SpatialPixelsDataFrame
    temp <- SpatialPoints(expand.grid(kd$x, kd$y))
    temp <- SpatialPixelsDataFrame(temp, data.frame(kde = as.vector(kd$z)))
    proj4string(temp) <- p4s
  #} else {
    # Create a terra raster object
    #temp <- rast(expand.grid(kd$x, kd$y), crs=p4s)
	#print(lims)
    #temp <- rast(nrows = n, ncols = n, xmin = lims[1], xmax = lims[2], ymin = lims[3], ymax = lims[4], crs=p4s)
    #values(temp) <- as.vector(t(kd$z))  # Transpose the matrix to match raster
  #}
  
  return(temp)
}




level.plot <- function(grd,shades,index=1,add=FALSE) 
{
		coords <- coordinates(grd)
		z = data.frame(grd)[,index]
	x = unique(coords[,1])
	y = unique(coords[,2])
	zz = z[!is.na(z)]
	if(missing(shades)) shades = auto.shading(zz,cutter=rangeCuts)
	levels = c(min(z,na.rm=TRUE),shades$breaks,max(z,na.rm=TRUE))
	z = matrix(z,length(x),length(y))
	storage.mode(z) = "double"
	if (!add)
	{
		xx = c(min(x),max(x))
		yy = c(min(y),max(y))
		plot(xx,yy,type='n',xaxt='n',yaxt='n',xaxs='i',yaxs='i',bty='n',xlab='',ylab='',asp=1)
	}
	cols = shades$cols
	.filled.contour(as.double(x),as.double(y),z,as.double(levels),col = cols)
}

