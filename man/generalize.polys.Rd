\name{generalize.polys}
\alias{generalize.polys}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{generalize.polys}
\description{
Generalises a \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object using the Douglas-Peuker algorithm}
\usage{
generalize.polys(sp, tol)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sp}{A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object.}
  \item{tol}{The weeding tolerance for the generalisation algorithm.}
}
\details{
 Returns an object of the same class as \code{sp}. Note that the algorithm is applied on a polygon-by-polygon,
 not edge-by-edge basis.  Thus edges in generalised polygons may not match perfectly.
}
\value{
 An object of class \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame}. Each polygon shape has been 
 generalized using the Douglas-Peuker algorithm.
}

\author{Chris Brunsdon}

\examples{
# Data for Georgia to use in example
data(georgia)
# Create an outline of Georgia
georgia.outline <- unionSpatialPolygons(georgia,rep(1,159))
plot(georgia.outline)
georgia.generalised <- generalize.polys(georgia.outline,0.1)
plot(georgia.generalised,add=TRUE,border='red')
}
