\name{level.plot}
\alias{level.plot}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Level plot for gridded data}
\description{
 Draws a level plot given a SpatialPixelsDataFrame,  an index and a shading scheme.
}
\usage{
level.plot(grd, shades, index=1, add=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{grd}{ A \code{spatialPixelsDataFrame} object. }
  \item{shades}{ A shading scheme created by \code{shading} or \code{auto.shading}. If omitted,  chosen automatically from \code{grd}.}
  \item{index}{Index giving the variable in \code{grd} to plot.}
  \item{add}{Whether to add the level plot to an existing plot.}
}
\details{
The function returns no value,  but draws a level plot on the current graphics device as a side effect.
}
\value{
None (see above).
}
\author{Chris Brunsdon, Binbin Lu}

\examples{
  \donttest{
# Data for New Haven to use in example
data(newhaven)
# Do the KDE
breach.dens = kde.points(breach,lims=tracts)
# Plot the result
level.plot(breach.dens)}
}
