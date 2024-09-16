\name{tornados}
\docType{data}
\alias{tornados}
\alias{torn}
\alias{torn2}


\title{US Tornado Touchdown Data}
\description{ Data set from NOAA's National Weather Service Indianapolis, IN Weather Forecast Office 6900 W. Hanna Ave.}

\usage{ 
data(tornados)}


%- maybe also 'usage' for other objects documented here.
\format{
\itemize{
  \item \strong{torn} Tornado Touchdown points \code{SpatialPointsDataFrame} - geographical projection 
  \item \strong{torn2} Tornado Touchdown points \code{SpatialPointsDataFrame } - equal area projection 
  }
}

\source{ \code{ http://www.crh.noaa.gov/ind/?n=svrgis }}

\examples{
# Read in the data
data(tornados)
# Split the plot in two
opar <- par(mfrow=c(2,1))
# Plot US states
plot(us_states)
# Add Locations of observation stations
plot(torn,add=TRUE,pch=16,col='red')
# Plot a histogram of year of observation next to this
hist(torn$YEAR)
par(opar)
}
