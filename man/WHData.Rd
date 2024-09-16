\name{WHData}
\docType{data}
\alias{WHD}
\alias{WHHP}
\alias{WHRD}
\alias{whp_sp}

\title{Data sets of Wuhan}
\description{
 Four geographic data sets of Wuhan are incoporated in this package}

\usage{data(WHData)}

%- maybe also 'usage' for other objects documented here.
\format{
\itemize{
  \item \strong{WHHP} House price data at the community level of Wuhan in a sf object 
  \item \strong{WHD} District boundary data of Wuhan 
  \item \strong{WHRD} Road network data of Wuhan 
 \item \strong{whp_sp} House price data at the community level of Wuhan in a SpatialPolygonsDataFrame object
 }
}

\examples{
# Read in the data
data(WHData)
choropleth(WHHP,"Avg_HP_avg")
}
