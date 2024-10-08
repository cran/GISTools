\name{Point in Polygon Counts}
\alias{poly.counts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Number of Points in Each Polygon}
\description{
Given a set of points,  and a set of polygons,  computes the number of points in each polygon.}
\usage{
poly.counts(pts, polys)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{pts}{A \code{SpatialPoints} or \code{SpatialPointsDataFrame} \code{sf} - POINT object .}
  \item{polys}{A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} or \code{sf} -POLYGON or - MULTIPOLYGON object.}
}
\value{
 A list of integers of the same length as the number of polygons in \code{polys},  giving the number of points from \code{pts}.
}

\author{Chris Brunsdon, Binbin Lu}

\examples{
# Data for New Haven to use in example
data(newhaven)
# How many breaches of peace in each census block?
n.breach = poly.counts(breach,blocks)
blocks@data$Count.per <- n.breach/poly.areas(blocks)
# Compute densities and map them
choropleth(blocks,"Count.per")
}

