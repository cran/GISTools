\name{newhaven}
\docType{data}
\alias{newhaven}
\alias{blocks}
\alias{breach}
\alias{famdisp}
\alias{burgres.f}
\alias{burgres.n}
\alias{places}
\alias{roads}
\alias{tracts}

\title{New Haven, Connecticut: Crime data with contextual information}
\description{
 Data set from New Haven (CT) crime web site containing point sources of some crimes,  plus roads,  railways and census block spatial data frames.
}
\usage{data(newhaven)}
%- maybe also 'usage' for other objects documented here.
\format{
  \itemize{
  \item \strong{blocks} Census blocks \code{SpatialPolygonsDataFrame } 
  \item \strong{roads} Roads \code{SpatialLinesDataFrame } 
  \item \strong{places} Place names \code{SpatialPointsDataFrame}
  \item \strong{breach} Breach of peace \code{SpatialPointsDataFrame } 
  \item \strong{famdisp} Family dispute \code{SpatialPointsDataFrame} 
  \item \strong{tracts} Census tracts \code{SpatialPolygonsDataFrame} 
  \item \strong{burgres.f} Residential Burglary (Forced) \code{SpatialPointsDataFrame} 
  \item \strong{burgres.n} Residential Burglary (Non-Forced) \code{SpatialPointsDataFrame} 
  }
}

\source{ \code{ http://www.newhavencrimelog.org/ }}

\examples{
# Read in map data for New Haven
data(newhaven)
# Plot census block boundaries
plot(blocks)
# Add a map scale
map.scale(534750,152000,miles2ft(2),"Miles",4,0.5,sfcol='red')
# ... and a title
title('New Haven (CT)')
}
