\name{Kernel Density Estimates From Points}
\alias{kde.points}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Kernel Density Estimates}
\description{
Given a set of points,  a bandwidth, a grid density and a frame, produce a kernel density estimate}
\usage{
kde.points(pts,h,n=200,lims=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pts}{A \code{SpatialPoints} or \code{SpatialPointsDataFrame} object.}
  \item{h}{A real number - the bandwidth of the KDE}
  \item{n}{An integer,  the output grid density - ie result is nxn grid}
  \item{lims}{A spatial object - the KDE grid will cover this,  if provided}
}
\value{
 A \code{SpatialPixelsDataFrame} containing the KDE.
}

\author{Chris Brunsdon}

\examples{
# Data for New Haven to use in example
data(newhaven)
# Do the KDE
breach.dens = kde.points(breach,lims=tracts)
# Plot the result
level.plot(breach.dens)
# Block out the part outside the study area
masker = poly.outer(breach.dens,tracts,extend=100); add.masking(masker)
# Plot census tract boundaries
plot(tracts,add=TRUE)
}


