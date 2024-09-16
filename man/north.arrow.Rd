\name{North Arrow}
\alias{north.arrow}
\title{Add a north arrow to a map}
\description{
 Draws a north arrow on a map.
}
\usage{
north.arrow(xb,yb,len,lab='NORTH',cex.lab=1,tcol='black',...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{xb}{ The \var{x}-centre (in map units) of the arrow base.}
  \item{yb}{ The \var{y}-centre (in map units) of the arrow base.}
  \item{len}{ The length (in map units) of the arrow base.}
  \item{lab}{The label for the arrow.}
  \item{cex.lab}{Scale factor for the label for the arrow.}
  \item{tcol}{ The colour of the label text. }
  \item{...}{ Other graphical parameters passed to the drawing of the arrow. }
}
\details{
Draws a north arrow on a map. The arrow itself is drawn using \code{polygon} and any extra parameters are passed to this call.
}
\value{
None.
}
\author{ Chris Brunsdon }
\seealso{\code{\link{map.scale}}}
\examples{
# Read in map data for New Haven
data(newhaven)
# Plot census block boundaries
plot(blocks)
# Add a north arrow
north.arrow(534750,152000,miles2ft(0.5),col='cyan')
# ... and a title
title('New Haven (CT)')
}
