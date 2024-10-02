\name{thematic.map}
\alias{thematic.map}
\title{ thematic.map}
\description{
 Draw thematic maps together with histograms with a \code{Spatial} or \code{sf} object, background layers according to variables and shading schemes.
}
\usage{
thematic.map(data, var.names, colorStyle = NULL, na.pos = "bottomright", bglyrs, bgStyle,
                         scaleBar.pos = "bottomright", mtitle = NULL, htitle = NULL,
                         legend = "Legend", legend.pos = "topright", cuts = 5, 
                         cutter = quantileCuts, horiz = FALSE, digits=2,...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{data}{ A \code{Spatial} or \code{sf} object.}
  \item{var.names}{ A vector of variable names to be mapped}
  \item{colorStyle}{ A vector of colors (including "red", "blue" and "green") or a color function, e.g. \code{rainbow}, \code{heat.colors}, \code{hcl.colors}, \code{terrain.colors} and \code{colors}}
  \item{na.pos}{ A 2-D coordinate or character to define the position of north arrow, i.e. "topright", "topleft", "bottomright", "bottomleft"}
  \item{bglyrs}{ A list of background layers of "Spatial" or "sf" objects}
  \item{bgStyle}{ A list of parameters for define the styles of background layers, e.g. list(col="grey", cex=1, lwd=1, pch=16, lty=1)}
  \item{scaleBar.pos}{ A 2-D coordinate or character to define the position of scale bar, i.e. "topright", "topleft", "bottomright", "bottomleft"}
  \item{mtitle}{ Title of each map}
  \item{htitle}{ Title of each histogram}
  \item{legend}{ Title of each legend for each map}
  \item{legend.pos}{  A 2-D coordinate or character to define the position of legend, i.e. "topright", "topleft", "bottomright", "bottomleft"}
  \item{cuts}{The number of classes.}
  \item{cutter}{ Function used to create the break points.  Can be user defined or a supplied \link[=quantileCuts]{cut function}.}
  \item{horiz}{ logical; if TRUE, set the legend horizontally rather than vertically }
  \item{digits}{ Number of digits kept for legend}
  \item{...}{ Arguments to be passed to methods}
}
\details{
The function returns no value,  but draws thematic maps together with histograms on the current graphics device as a side effect.
}
\value{
None (see above).
}
\author{Binbin Lu}
\seealso{\code{\link{choropleth}}, \code{\link{choro.legend}}, \code{\link{auto.shading}}, \code{\link{shading}}.}
\examples{
data(newhaven)
# Single map
thematic.map(blocks, var.names="POP1990", horiz = FALSE, na.pos = "topleft", 
            scaleBar.pos = "bottomright", legend.pos = "bottomleft",
            colorStyle = "red")

#Multiple maps and different colors
thematic.map(blocks, var.names=c("P_35_44","P_25_34", "POP1990"), 
horiz =FALSE, na.pos = "topleft", scaleBar.pos = "bottomright", 

legend.pos = "bottomleft",colorStyle =hcl.colors)
thematic.map(blocks, var.names=c("P_35_44","P_25_34", "POP1990"), 

horiz =FALSE, na.pos = "topleft", scaleBar.pos = "bottomright",
 legend.pos = "bottomleft", colorStyle =c("red", "blue", "na"))
# Use coordinate to define the legend
data(WHData)
thematic.map(whp_sp, var.names="Avg_Green_", colorStyle ="red",
             na.pos = "topleft", scaleBar.pos = "bottomleft", 
             legend.pos = c(544000, 3380000),cuts=7, 
             cutter=rangeCuts)
thematic.map(WHHP, var.names=c("Avg_HP_avg", "Avg_Green_"), colorStyle =c("red", "blue"),
             na.pos = "topleft", scaleBar.pos = "bottomleft", 
             legend.pos = c(544000, 3380000),cuts=7, 
             cutter=rangeCuts)
}
