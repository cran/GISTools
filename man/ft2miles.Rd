\name{Unit Conversion}
\alias{ft2miles}
\alias{miles2ft}
\alias{ft2km}
\alias{km2ft}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Distance Units Conversion }
\description{
 Convert between different distance units - all functions take the form \code{ xx2yy } where \code{ xx } is the unit to be converted from and \code{ yy } is the unit to be converted to.}
\usage{
ft2miles(x)
miles2ft(x)
ft2km(x)
km2ft(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{A quantity in units to be converted from}}
\value{
The value of \code{x} converted to the new units.  In the example below the conversions are from feet to miles and feet to kilometers  (hence functions are \code{ft2miles} and \code{ft2km}).
}
\author{Chris Brunsdon}
\examples{
# How many miles is 10,000 feet?
ft2miles(10000)
# How about in kilometers?
ft2km(10000)
}


