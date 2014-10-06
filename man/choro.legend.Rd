\name{choro.legend}
\alias{choro.legend}
\title{choro.legend}
\description{
 Draw a legend for a choropleth map.
}
\usage{
choro.legend(px, py, sh, under = "under", over = "over", 
  between = "to", fmt = "\%g", cex=1, ...)
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
  \item{cex}{ Relative size of text in the legend. }
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
# Read in map data and compute a rate for mapping
sids <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], 
	proj4string=CRS("+proj=longlat +ellps=clrk66"))
sids.rate=10000*sids@data[,10]/sids@data[,9]
# Create the shading scheme,  plot a choropleth map and add a legend
shades = auto.shading(sids.rate)
choropleth(sids,sids.rate,shades)
choro.legend(-83.77,37.87,shades,fmt="\%4.1f",cex=0.8,title='Rate per 10,000')
}
