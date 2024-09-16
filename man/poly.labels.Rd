\name{Polygon Label Points}
\alias{poly.labels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Number of Points in Each Polygon}
\description{
Given a set of polygons,  returns the label point for each polygon in a SpatialPoints or a sf POINT object.}
\usage{
poly.labels(polys)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{polys}{A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame}  or \code{sf} -POLYGON or - MULTIPOLYGON object.}
}
\value{
 SpatialPoints or sf POINT object containing the label point for each polygon, respectively.
}

\author{Chris Brunsdon, Binbin Lu}


