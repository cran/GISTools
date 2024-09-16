\name{Create a `mask' polygon}
\alias{poly.outer}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Create a masking polygon to block out graphics outside a region. }
\description{
 Takes a polygon object and creates a new polygon whose outline is rectangular, but has a hole shaped like the input polygon cut into it. This is useful for plotting surfaces defined over a study area,  but masking the values outside of the area.  It is designed to work with pixel images,  so that the mask covers up all parts of the image not in the input polygon.
} 
\usage{
poly.outer(exo.object,input.poly,extend=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{exo.object}{The object extending beyond \code{input.poly} that is to be masked.  This is required to ensure that 
  the external rectangle of the mask will be large enough.}
  \item{input.poly}{The polygon used to make the hole in the mask.}
  \item{extend}{A buffer used to extend the mask if it is required to be larger than \code{exo.object}}
}
\value{A polygon object whose outline is rectangular,  but having holes cut into it in the shape of \code{input.poly}}


\seealso{\code{\link{add.masking}}, \code{\link{kde.points}}. }


\author{Chris Brunsdon, Binbin Lu}

\examples{
  \donttest{
# Data for New Haven to use in example
data(newhaven)
# Do the KDE
breach.dens = kde.points(breach,lims=tracts)
# Plot the result
level.plot(breach.dens)
# Block out the part outside the study area
masker = poly.outer(breach.dens,tracts,extend=100); add.masking(masker)
# Plot census tract boundaries
plot(tracts,add=TRUE)}
}





