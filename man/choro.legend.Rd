\name{choro.legend}
\alias{choro.legend}
\title{choro.legend}
\description{
 Draw a legend for a choropleth map.
}
\usage{
choro.legend(px, py, sh, under = "under", over = "over", 
  between = "to", fmt = "\%g", cex=1, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{px}{ \var{x} coordinate of legend location }
  \item{py}{ \var{y} coordinate of legend location}
  \item{sh}{ Shading scheme object used as basis for the legend }
  \item{under}{ What to write in front of the lowest choropleth class upper limit.}
  \item{over}{ What to write in front of the highest choropleth class lower limit. }
  \item{between}{ What to write between the upper and lower limits of intermediate chropleth classes. }
  \item{fmt}{C style format for values stated in above choroplth class limits. }
  \item{cex}{ Relative size of text in the legend. }
  \item{\dots}{Other arguments, passed on to the generic \code{\link{legend}} function. }
}
\details{
Returns no value,  but draws a choropleth map legend on the current graphics device as a side effect
}
\value{
None (see above)
}
\author{ Chris Brunsdon }
\seealso{\code{\link{choropleth}}, \code{\link{auto.shading}}, \code{\link{shading}}. }
\examples{
# Read in map data and compute a rate for mapping
# Try the sf class
data(WHData)
shades = auto.shading(WHHP[["Avg_HP_avg"]],n=7)
dev.new(width = 16, height = 12)
choropleth(sp = WHHP,v="Avg_HP_avg",shading=shades)
choro.legend(548871.4, 3377000, shades,title='Average house price')
#Try the Spatial object
shades = auto.shading(whp_sp@data[["Avg_HP_avg"]],n=6)
dev.new(width = 16, height = 12)
choropleth(sp = whp_sp,v="Avg_HP_avg",shading=shades)
choro.legend(548871.4, 3377000,  shades,title='Average house price')
}
