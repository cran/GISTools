\name{shading}
\alias{shading}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Shading }
\description{
 Creates an object of class \code{shading} by directly specifying break values and (optionally) colours.
}
\usage{
shading(breaks, cols = brewer.pal(length(breaks), "Reds"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{breaks}{The break points}
  \item{cols}{The shading colours - there should be one more of these than break points.}
}
\value{
An object of class \code{shading}.
}
\author{Chris Brunsdon}
\section{Warning}{At the moment,  the it is assumed that the number of shading colours is one more than the break points,  but this is not checked.}
\seealso{ \code{\link{choropleth}},\code{\link{choro.legend}} }
\examples{
# Open a new graphics window - default is not a nice shape for a US map
if (.Platform$GUI == "AQUA") {
  quartz(w=10,h=8) } else  {
  x11(w=10,h=8) }
# Read in map data and compute a rate for mapping
sids <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], proj4string=CRS("+proj=longlat +ellps=clrk66"))
sids.rate=10000*sids@data[,10]/sids@data[,9]
shades = shading(breaks=c(15,30,45,60,75),cols=brewer.pal(6,'YlGn'))
choropleth(sids,sids.rate,shades)
choro.legend(-83.77,37.87,shades,fmt="\%4.0f",title='Rate per 10,000')
}

