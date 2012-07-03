\name{choro.legend}
\alias{choro.legend}
\title{choro.legend}
\description{
 Draw a legend for a choropleth map.
}
\usage{
choro.legend(px, py, sh, under = "under", over = "over", between = "to", fmt = "\%g", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{px}{ \var{x} coordinate of legend location }
  \item{py}{ \var{y} coordinate of legend location}
  \item{sh}{ Shading scheme object used as basis for the legend }
  \item{under}{ What to write in front of the lowest choropleth class upper limit.}
  \item{over}{ What to write in front of the highest choropleth class lower limit. }
  \item{between}{ What to write between the upper and lower limits of intermediate chropleth classes. }
  \item{fmt}{C style format for values stated in above choroplth class limits. }
  \item{\dots}{Other arguments, passed on to the generic \code{\link{legend}} function. }
}
\details{
Returns no value,  but draws a choropleth map legend on the current graphics device as a side effect
}
\value{
None (see above)
}
\author{ Chris Brunsdon }
\seealso{\code{\link{choropleth}}, \code{\link{auto.shading}}, \code{\link{shading}}. }
\examples{
# Open a new graphics window - default is not a nice shape for a US map
if (.Platform$GUI == "AQUA") {
  quartz(w=10,h=8) } else  {
  x11(w=10,h=8) }
# Read in map data and compute a rate for mapping
sids <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], proj4string=CRS("+proj=longlat +ellps=clrk66"))
sids.rate=10000*sids@data[,10]/sids@data[,9]
# Create the shading scheme,  plot a choropleth map and add a legend
shades = auto.shading(sids.rate)
choropleth(sids,sids.rate,shades)
choro.legend(-83.77,37.87,shades,fmt="\%4.1f",title='Rate per 10,000')
# Don't forget to close the map window if you want to revert to old graphics device
}