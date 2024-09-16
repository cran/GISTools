\name{map.scale}
\alias{map.scale}
\title{map.scale}
\description{
 Draws a scale bar on a map.
}
\usage{
map.scale(xc,yc,len,units,ndivs,subdiv=1,tcol='black',scol='black',sfcol='black')
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xc}{ The \var{x}-centre (in map units) of the scale bar }
  \item{yc}{ The \var{y}-centre (in map units) of the scale bar }
  \item{len}{ The length (in map units) of the scale bar }
  \item{units}{ String specifying the name of the units for the scale bar }
  \item{ndivs}{The number of divisions (units marked) on the scale }
  \item{subdiv}{The fraction of \code{units} used to step along the divisions }
  \item{tcol}{ The colour of text on the scale bar. }
  \item{scol}{ The colour of the scale bar itself. }
  \item{sfcol}{ The colour of the filled rectangles in the scale bar. }
}
\details{
Draws an alternating bar scale on a map. Returns no value.
}
\value{
None (see above)
}
\author{ Chris Brunsdon }
\seealso{\code{\link{choro.legend}}}
\examples{
# Read in map data for New Haven
data(newhaven)
# Plot census block boundaries
plot(blocks)
# Add a map scale
map.scale(534750,152000,miles2ft(2),"Miles",4,0.5,sfcol='red')
# ... and a title
title('New Haven (CT)')
}
