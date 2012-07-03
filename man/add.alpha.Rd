\name{Create Transparency}
\alias{add.alpha}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{ Add transparency to a hex-defined colour }
\description{
 Takes a colour defined in hex format as \code{#XXXXXX} and adds a two transparency bytes \code{XX} based on a number from 0 to 1.  Its main use is to make RColorBrewer palettes transparent.
} 
\usage{
add.alpha(hex.color.list,alpha)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{hex.color.list}{A list of strings defining solid colors in six byte format.}
  \item{alpha}{A value (or list of values) from 0 to 1 specifying transparency.}
}
\value{A list of strings defining transparent colours in eight byte format.}

\author{Chris Brunsdon}
\examples{
# Make a list of semi-transparent RColorBrewer colours,  based on Brewer's Red palette with 5 shades
add.alpha(brewer.pal(5,'Reds'),0.5)
}


