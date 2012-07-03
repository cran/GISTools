\name{Polygon Areas}
\alias{poly.areas}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Area of Each Polygon}
\description{
Given a set of polygons,  returns the area of each polygon.}
\usage{
poly.areas(polys)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{polys}{A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object.}
}
\value{
 A list of areas of the same length as the number of polygons in \code{polys}.
}

\author{Chris Brunsdon}

\examples{
# Data for New Haven to use in example
data(newhaven)
# What is the area each census block?
poly.areas(blocks)
}

