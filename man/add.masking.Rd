\name{Add masking around an image}
\alias{add.masking}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Draw a mask around a Grid Based Image }
\description{
 Takes an `mask' type polygon object - basically a rectangle with a polygon hole cut through it - and draws this over an image.  This has the effect of only showing the image inside the hole.  This is useful for plotting surfaces defined over a study area,  but masking the values outside of the area.
} 
\usage{
add.masking(maskPoly,color)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{maskPoly}{A masking polygon as described above.}
  \item{color}{Colour of the mask.  Defaults to white,  but for example,  sea could be shown as blue.}
}
\value{None}
\details{
Returns no value,  but draws a mask on the current graphics device as a side effect
}

\seealso{\code{\link{poly.outer}}, \code{\link{kde.points}}. }

\author{Chris Brunsdon}

\examples{
# Data for New Haven to use in example
data(newhaven)
# Do the KDE
breach.dens = kde.points(breach,lims=tracts)
# Plot the result
level.plot(breach.dens)
# Block out the part outside the study area
masker = poly.outer(breach.dens,tracts,extend=100); add.masking(masker)
# Plot census tract boundaries
plot(tracts,add=TRUE)
}





