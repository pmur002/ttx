\name{ttxGlyphNames}
\alias{ttxGlyphNames}
\alias{ttxGlyphIndex}
\alias{ttxGlyphWidth}
\alias{ttxGlyphHeight}
\alias{ttxGlyphBounds}
\title{
  Access Glyph Information
}
\description{
  Access glyph information within a True Type or Open Type font.
}
\usage{
ttxGlyphNames(fontfile)
ttxGlyphIndex(name, fontfile)
ttxGlyphWidth(index, fontfile)
ttxGlyphHeight(index, fontfile)
ttxGlyphBounds(index, fontfile)
}
\arguments{
  \item{fontfile}{Path to a font file.}
  \item{name}{Name of a glyph.}
  \item{index}{Numeric index of a glyph.}
}
\value{
  \code{ttxGlyphNames()} returns a character vector.
  
  \code{ttxGlyphIndex()} returns a numeric index.

  \code{ttxGlyphWidth()} returns two numeric values: the advance width
  and the left-side bearing.

  \code{ttxGlyphHeight()} returns two numeric values: the advance height
  and the top-side bearing (for vertical typesetting).

  \code{ttxGlyphBounds()} returns the tight bounding box for the glyph.
}
\author{
  Paul Murrell
}
\keyword{ dplot }

