\name{ttxFontFamily}
\alias{ttxFontFamily}
\alias{ttxFontWeight}
\alias{ttxFontStyle}
\title{
  Query Font Properties
}
\description{
  Get the name, weight, and style of a font.
}
\usage{
ttxFontFamily(fontfile)
ttxFontWeight(fontfile)
ttxFontStyle(fontfile)
}
\arguments{
  \item{fontfile}{Path to a font file.}
}
\value{
  \code{ttxFontFamily()} and \code{ttxFontStyle()} return a character value.
  
  \code{ttxGetTable()} returns an XML document.
}
\author{
  Paul Murrell
}
\keyword{ dplot }

