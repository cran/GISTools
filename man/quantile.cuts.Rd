\name{cut function}
\alias{quantileCuts}
\alias{sdCuts}
\alias{rangeCuts}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Cut functions}
\description{
 Helper functions for \code{\link{auto.shading}}.  Given a variable to be mapped, a number of classes and possibly some more \code{params},  returns a list of break values.  There should be one less break value than the number of classes.
}
\usage{
quantileCuts(x, n = 5, params = NA)
sdCuts(x, n = 5, params = NA)
rangeCuts(x, n = 5, params = NA)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{The variable to be mapped.}
  \item{n}{The number of classes.}
  \item{params}{Extra params for individual cut functions.}
}
\value{
An ordered list of the break values between classes
}
\author{Chris Brunsdon}
\note{
The only cut function using \code{params} is \code{quantileCuts},  where it is used to specify a list of quantile values - useful if they are not evenly spaced.
}
\seealso{\code{\link{auto.shading}}}
