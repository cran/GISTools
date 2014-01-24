\name{choropleth}
\alias{choropleth}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ choropleth}
\description{
 Draws a choropleth map given a spatialPolygons object,  a variable and a shading scheme.
}
\usage{
choropleth(sp, v, shading = auto.shading(v), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{ A \code{spatialPolygons} or \code{spatialPolygonsDataFrame} object. }
  \item{v}{ The variaqble to be mapped.  Must have the same number of elements as \code{s} has polygons.}
  \item{shading}{ A shading scheme created by \code{shading} or \code{auto.shading}.}
  \item{\dots}{Additional parameters to be passed on to the \code{plot} method for \code{sp}.}
}
\details{
The function returns no value,  but draws a choropleth map on the current graphics device as a side effect.
}
\value{
None (see above).
}
\author{Chris Brunsdon}
\seealso{\code{\link{choro.legend}}, \code{\link{auto.shading}}, \code{\link{shading}}.}
\examples{
# Read in map data and compute a rate for mapping
sids <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], 
	proj4string=CRS("+proj=longlat +ellps=clrk66"))
sids.rate=10000*sids$SID74/sids$BIR74
# Create the shading scheme,  plot a choropleth map 
shades = auto.shading(sids.rate,cols=brewer.pal(5,'Blues'))
choropleth(sids,sids.rate,shades)
}
