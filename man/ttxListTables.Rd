\name{ttxListTables}
\alias{ttxListTables}
\alias{ttxGetTable}
\title{
  Access Font Tables
}
\description{
  List and access the tables within a True Type or Open Type font.
}
\usage{
ttxListTables(fontfile)
ttxGetTable(table, fontfile)
}
\arguments{
  \item{fontfile}{Path to a font file.}
  \item{table}{Name of a font table.}
}
\value{
  \code{ttxListTables()} returns a character vector.
  
  \code{ttxGetTable()} returns an XML document.
}
\author{
  Paul Murrell
}
\keyword{ dplot }

