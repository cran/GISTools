\name{vulgaris}
\docType{data}
\alias{vulgaris}
\alias{vulgaris2}
\alias{us_states}
\alias{us_states2}

\title{Phenology data for North American lilacs}
\description{
 Data set from Schwartz, M.D. and J.M. Caprio, 2003, North American First Leaf and First Bloom Lilac Phenology Data, IGBP PAGES/World Data Center for Paleoclimatology 
Data Contribution Series # 2003-078. NOAA/NGDC Paleoclimatology Program, Boulder CO, USA.}

\usage{ data(vulgaris) 
vulgaris2
us_states
us_states2}

%- maybe also 'usage' for other objects documented here.
\format{
\itemize{
  \item \strong{vulgaris} Syringa Vulgaris Observation Stations \code{SpatialPointsDataFrame} - geographical projection 
  \item \strong{vulgaris2} Syringa Vulgaris Observation Stations \code{SpatialPointsDataFrame } - equal area projection 
  \item \strong{us_states} States of US \code{SpatialPolygonsDataFrame } - geographical projection 
 \item \strong{us_states2} States of US \code{SpatialPolygonsDataFrame } - equal area projection 
 }
}

\source{ \code{ http://www.ncdc.noaa.gov/paleo/phenology.html }}

\examples{
# Read in the data
data(vulgaris)
# Split the plot in two
par(mfrow=c(2,1))
# Plot US states
plot(us_states)
# Add Locations of observation stations
plot(vulgaris,add=TRUE,pch=16,col='red')
# Plot a histogram of year of observation next to this
hist(vulgaris$Year)
}
