\name{auto.shading}
\alias{auto.shading}
%\alias{sdCuts}
%\alias{quantileCuts}
%\alias{rangeCuts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ auto.shading }
\description{
 Creates an object of class \code{shading} automatically,  given a choropleth variable to be mapped.
}
\usage{
auto.shading(x, digits = 2, cutter = quantileCuts, n = 5, 
	params = NA, cols = brewer.pal(n, "Reds"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The variable to be mapped.}
  \item{digits}{The number of significant digits to round the class limits to. }
  \item{cutter}{Function used to create the break points.  Can be user defined or a supplied \link[=quantileCuts]{cut function}.}
  \item{n}{The number of classes.  The should be one more than the number of break points.}
  \item{params}{Other parameters to be passed to the cut function.}
  \item{cols}{List of colours for shading each class. \code{length(cols)} should be equal to \code{n}.}
}
\details{
 Returns an object of class \code{shading},  as set out below:
}
\value{
 An object of class \code{shading},  having the following list elements:
  \item{breaks }{Break points between choropleth classes.  \code{length(cols)}  }
  \item{cols }{Colours to shade in each class. \code{length(cols)} should be one more than  \code{length(breaks)}   }
}

\author{Chris Brunsdon}
\seealso{\code{\link{choropleth}},\code{\link{shading}},\code{\link{choro.legend}}.}
\examples{
# Read in map data and compute a rate for mapping
sids <- readShapePoly(system.file("shapes/sids.shp", package="maptools")[1], 
	proj4string=CRS("+proj=longlat +ellps=clrk66"))
sids.rate=10000*sids$SID74/sids$BIR74
# Create the shading scheme,  plot a choropleth map and add a legend
shades = auto.shading(sids.rate,n=6)
choropleth(sids,sids.rate,shades)
choro.legend(-83.77,37.87,shades,fmt="\%4.1f",title='Rate per 10,000')
# Now again with a different set of class intervals and colours
shades = auto.shading(sids.rate,n=6,cutter=rangeCuts,cols=brewer.pal(6,'Greens'))
choropleth(sids,sids.rate,shades)
choro.legend(-83.77,37.87,shades,fmt="\%4.1f",title='Rate per 10,000')
}
