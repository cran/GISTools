\name{georgia}
\docType{data}
\alias{georgia}
\alias{georgia2}
\alias{georgia.polys}
\title{Georgia Social and Economic Data by County}
\description{ Polygon Data Frame as used in the Brunsdon, Fotheringham & Charlton GWR book, with further variable median income (MedInc)}

\usage{ data(georgia) 
georgia
georgia2
}

%- maybe also 'usage' for other objects documented here.
\format{
 \itemize{
  \item \strong{georgia}  Georgia polygons \code{SpatialPolygonsDataFrame} - geographical projection 
  \item \strong{georgia2} Georgia polygons \code{SpatialPolygonsDataFrame } - equal area projection 
  \item \strong{georgia.polys} Georgia polygons in list format - equal area projection 
  }
}

\examples{
# Read in the data
data(georgia)
# Make a map of median income
choropleth(georgia2,georgia2$MedInc)
}
