\name{choropleth}
\alias{choropleth}
\title{ choropleth}
\description{
 Draws a choropleth map given a spatialPolygons object,  a variable and a shading scheme.
}
\usage{
choropleth(sp, v, shading, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{ A \code{SpatialPolygonsDataFrame} or \code{sf} -POLYGON or - MULTIPOLYGON object. }
  \item{v}{ The variable name to be mapped and must included in the data.}
  \item{shading}{ A shading scheme created by \code{shading} or \code{auto.shading}.}
  \item{\dots}{Additional parameters to be passed on to the \code{plot} method for \code{sp}.}
}
\details{
The function returns no value,  but draws a choropleth map on the current graphics device as a side effect.
}
\value{
None (see above).
}
\author{Chris Brunsdon, Binbin Lu}
\seealso{\code{\link{choro.legend}}, \code{\link{auto.shading}}, \code{\link{shading}}.}
\examples{
# Read in map data and compute a rate for mapping
# Try the sf class
data(WHData)
shades = auto.shading(WHHP[["Avg_HP_avg"]],n=7)
dev.new(width = 16, height = 12)
choropleth(sp = WHHP,v="Avg_HP_avg",shading=shades)
choro.legend(548871.4, 3377000, shades,title='Average house price')
#Try the Spatial object
shades = auto.shading(whp_sp@data[["Avg_HP_avg"]],n=6)
dev.new(width = 16, height = 12)
choropleth(sp = whp_sp,v="Avg_HP_avg",shading=shades)
choro.legend(548871.4, 3377000,  shades,title='Average house price')
}
