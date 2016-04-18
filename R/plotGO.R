##' Plots a GO subgraph with all the \code{goids} of interest
##' up to the root of the ontology.
##'
##' @title Plots a GO sub-graph
##' @param goids A \code{character} vector of GO identifiers.
##' @param use One of \code{"RamiGO"} or \code{"Rgraphviz"} that
##'     specifies which plot (and which format) should be generated.
##' @param showPlot A \code{logical} defining if the plot should be
##'     displayed. Default is \code{TRUE}.
##' @param params A \code{list} with additional parameters to be
##'     passed to other functions
##' @return This function is primarily used for its side effects of
##'     generating plots. When using \code{"RamiGO"}, a
##'     \code{character} containing the SVG code is invisibly
##'     returned. For \code{"Rgraphviz"}, the GO sub-graph is returned
##'     as a \code{\link[graph]{graphNEL}} object.
##' @author Laurent Gatto
##' @export
##' @examples
##' goids <-c("GO:0005739", "GO:0005773", "GO:0005783", "GO:0032588")
##' plotGO(goids)
##' plotGO(goids, use = "Rgraphviz")
plotGO <- function(goids,
                   use = c("RamiGO", "Rgraphviz"),
                   showPlot = TRUE,
                   params = list(edge.labels=TRUE, ## rgraphviz
                                 color="#8FBDDAFF", filename="gotree",
                                 picType="svg" ## ramigo
                                 )) {
    ns <- goNamespace(goids)
    for (i in seq_along(goids))
        stopifnot(validGO(goids[i], ns[i]))
    if (length(uns <- unique(ns)) != 1)
        stop("Can only plot terms from a single namespace, got ",
             length(uns), ": ",
             paste(uns, collapse = ", "), ".")
    use <- match.arg(use)
    if (use == "Rgraphviz") {
        requireNamespace("Rgraphviz") || stop("Rgraphviz is required. Alternatively use 'RamiGO'.")
        requireNamespace("graph") ## depends on Rgraphviz
        gr <- buildGOgraph(goids, uns)
        graph::nodeRenderInfo(gr) <- list(fillcolor="transparent",
                                          shape="ellipse")
        graph::nodeRenderInfo(gr)$fillcolor[graph::nodes(gr) %in% goids] <- "#8FBDDAFF"
        tmp <- paste(graph::nodes(gr),'\\\n',
                     sapply(as.list(GOTERM[graph::nodes(gr)]), function(x) x@Term),
                     sep = "")
        tmp <- gsub(" ", "\\\\\n", tmp)
        names(tmp) <- graph::nodes(gr)
        graph::nodeRenderInfo(gr)$label <- tmp
        graphAttrs <- Rgraphviz::getDefaultAttrs(layoutType = "dot")
        graphAttrs$graph$rankdir <- "BT"
        graph::edgeRenderInfo(gr) <- list(label = "")
        if (params$edge.labels) {
            edglab <- sapply(names(gr@edgeData),
                             function(xx) {
                                 x <- unlist(strsplit(xx,"\\|"))
                                 edges <- GOCCPARENTS[[x[1]]]
                                 (names(edges[edges == x[2]]))
                             })
            names(edglab) <- names(graph::edgeRenderInfo(gr)$label)
            graph::edgeRenderInfo(gr)$label <- edglab
        }
        res <- Rgraphviz::agopen(graph = gr,
                                 name = "graph",
                                 attrs = graphAttrs,
                                 nodeAttrs = graph::nodeRenderInfo(gr),
                                 edgeAttrs = graph::edgeRenderInfo(gr))
        if (showPlot)
            graph::plot(res)
        invisible(gr)
    } else { ## RamiGO
        res <- RamiGO::getAmigoTree(goIDs = goids,
                                    color = params$color,
                                    filename = params$filename,
                                    picType = params$picType)
        if (showPlot) {
            url <- paste("file://", getwd(), "/",
                         params$filename, ".", params$picType,
                         sep="")
            browseURL(url)
        }
        invisible(res)
    }
}

