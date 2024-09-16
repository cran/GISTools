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

\author{Chris Brunsdon, Binbin Lu}
\seealso{\code{\link{choropleth}},\code{\link{shading}},\code{\link{choro.legend}}.}
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
