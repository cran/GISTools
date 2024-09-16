\name{Computational Inference from Point Data}
\alias{jitter.points}
\alias{bstrap.points}
\title{ Bootstrap and Kernel Bootstrap from Points }
\description{
 Operations for bootstrapping and kernel bootstrapping based on point data.\code{ bstrap.points } sample \code{n} points with replacement from a sample - and \code{ jitter.points } adds a Gaussian displacement to each point in a data set.  Applying a jitter to a bootstrap effectively creates a kernel bootstrap operation.}

\usage{
jitter.points(pts,scl)
bstrap.points(pts)
}
\arguments{
  \item{pts}{A \code{SpatialPointsDataFrame} or \code{sf} - POINT object}
  \item{scl}{A scale parameter - basically the standard deviation of the random Gaussian displacement}}
\value{
A \code{SpatialPointsDataFrame} - with either a sample without replacement or a replica of the input data with displacements.
}
\author{Chris Brunsdon, Binbin Lu}
\examples{
\donttest{
data(newhaven)
plot(blocks)
for (i in 1:20) plot(jitter.points(breach,150),add=TRUE,pch=1,col='red')
}
}


