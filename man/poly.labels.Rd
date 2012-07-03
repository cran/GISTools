\name{Polygon Label Points}
\alias{poly.labels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Number of Points in Each Polygon}
\description{
Given a set of polygons,  returns the label point for each polygon in a SpatialPoints object.}
\usage{
poly.labels(polys)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{polys}{A \code{SpatialPolygons} or \code{SpatialPolygonsDataFrame} object.}
}
\value{
 SpatialPoints object containing the label point for each polygon.
}

\author{Chris Brunsdon}

\examples{
# Data for New Haven to use in example
data(newhaven)
# How many breaches of peace in each census block?
n.breach = poly.counts(breach,blocks)
# Compute densities and map them
choropleth(blocks,n.breach/blocks$AREA)
}

