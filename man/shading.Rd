\name{shading}
\alias{shading}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Shading }
\description{
 Creates an object of class \code{shading} by directly specifying break values and (optionally) colours.
}
\usage{
shading(breaks, cols = brewer.pal(length(breaks), "Reds"))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{breaks}{The break points}
  \item{cols}{The shading colours - there should be one more of these than break points.}
}
\value{
An object of class \code{shading}.
}
\author{Chris Brunsdon}
\section{Warning}{At the moment,  the it is assumed that the number of shading colours is one more than the break points,  but this is not checked.}
\seealso{ \code{\link{choropleth}},\code{\link{choro.legend}} }
