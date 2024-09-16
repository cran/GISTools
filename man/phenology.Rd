\name{phenology}
\docType{data}
\alias{phenology}
\alias{chinensis}
\alias{chinensis2}
\title{Phenology data for North American lilacs}
\description{
 Data set from Schwartz, M.D. and J.M. Caprio, 2003, North American First Leaf and First Bloom Lilac Phenology Data, IGBP PAGES/World Data Center for Paleoclimatology 
Data Contribution Series # 2003-078. NOAA/NGDC Paleoclimatology Program, Boulder CO, USA.}

\usage{ data(phenology)}

%- maybe also 'usage' for other objects documented here.
\format{
 \itemize{
  \item \strong{chinensis} Syringa Chinensis Observation Stations \code{SpatialPointsDataFrame} - geographical projection 
  \item \strong{chinensis2} Syringa Chinensis Observation Stations \code{SpatialPointsDataFrame } - equal area projection  
  \item \strong{us_states} States of US \code{SpatialPolygonsDataFrame } - geographical projection 
  \item \strong{us_states2} States of US \code{SpatialPolygonsDataFrame } - equal area projection 
 }
}

\source{ \code{ http://www.ncdc.noaa.gov/paleo/phenology.html }}

\examples{
# Read in the data
data(phenology)
# Split the plot in two
opar <- par(mfrow=c(2,1))
# Plot US states
plot(us_states2)
# Add Locations of observation stations
plot(chinensis2,add=TRUE,pch=16,col='red')
# Plot a histogram of year of observation next to this
hist(chinensis2$Year)
par(opar)
}
