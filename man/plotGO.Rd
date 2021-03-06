% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotGO.R
\name{plotGO}
\alias{plotGO}
\title{Plots a GO sub-graph}
\usage{
plotGO(goids, use = c("RamiGO", "Rgraphviz"), showPlot = TRUE,
  params = list(edge.labels = TRUE, color = "#8FBDDAFF", filename = "gotree",
  picType = "svg"))
}
\arguments{
\item{goids}{A \code{character} vector of GO identifiers.}

\item{use}{One of \code{"RamiGO"} or \code{"Rgraphviz"} that
specifies which plot (and which format) should be generated.}

\item{showPlot}{A \code{logical} defining if the plot should be
displayed. Default is \code{TRUE}.}

\item{params}{A \code{list} with additional parameters to be
passed to other functions}
}
\value{
This function is primarily used for its side effects of
    generating plots. When using \code{"RamiGO"}, a
    \code{character} containing the SVG code is invisibly
    returned. For \code{"Rgraphviz"}, the GO sub-graph is returned
    as a \code{\link[graph]{graphNEL}} object.
}
\description{
Plots a GO subgraph with all the \code{goids} of interest
up to the root of the ontology.
}
\examples{
goids <-c("GO:0005739", "GO:0005773", "GO:0005783", "GO:0032588")
plotGO(goids)
plotGO(goids, use = "Rgraphviz")
}
\author{
Laurent Gatto
}

