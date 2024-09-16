\name{generalize.polys}
\alias{generalize.polys}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{generalize.polys}
\description{
Generalises a \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} or a \code{sf} -POLYGON or - MULTIPOLYGON object using the Douglas-Peuker algorithm}
\usage{
generalize.polys(sfo, preserveTopology, dTolerance)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sfo}{A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} or a \code{sf} -POLYGON or - MULTIPOLYGON object.}
  \item{preserveTopology}{logical; carry out topology preserving simplification? May be specified for each, or for all feature geometries. Note that topology is preserved only for single feature geometries, not for sets of them.}
  \item{dTolerance}{numeric; tolerance parameter, specified for all or for each feature geometry.}
}
\details{
 Returns an object of the same class as \code{sp}. Note that the algorithm is applied on a polygon-by-polygon,
 not edge-by-edge basis.  Thus edges in generalised polygons may not match perfectly.
}
\value{
 An object of class \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame}. Each polygon shape has been 
 generalized using the Douglas-Peuker algorithm.
}

\author{Chris Brunsdon, Binbin Lu}

\examples{
# Data for Georgia to use in example
\donttest{
data(WHData)
WH.outline <- st_union(WHHP)
WH.generalised <- generalize.polys(WH.outline,TRUE,0.1)
plot(st_geometry(WHHP))
plot(st_geometry(WH.generalised),add=TRUE,border='red',lwd=2)
}
}
