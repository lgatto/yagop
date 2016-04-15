# Yet Another Gene Ontology Package

## Current functionality


```r
library("yagop")
ls("package:yagop")
```

```
## [1] "validGO"
```

## Anticipated functionality

Given a vector of GO ids, return a list of children, descendants,
parents and ancestors. For example:



```r
##' @title Get the children of GO identifiers
##' @param x A \code{vector} of GO identifiers
##' @param simplify A \code{logical} of length 1 defining if the
##'     return value should be simplified to a vector if \code{x} is
##'     of length 1. Default is \code{TRUE}.
##' @param namespace An optional GO namespace to search for
##'     children. Either of \code{"CC"}, \code{"MF"} or
##'     \code{"BP"}. If missing, all namespace will be searched.
##' @return If \code{x} is of length 1 and \code{simplify} is
##'     \code{TRUE}), a \code{vector}, otherwise a \code{list} of
##'     length equal to \code{length{x}}.
##' @author Laurent Gatto
##' @export
##' @examples
##' goids <- c("GO:0005739", "GO:0005773", "GO:0005783", "GO:0032588")
##' chldrn <- children(goids)
##' ## remove those without any children
##' chldrn <- children[!is.na(cildrn)]
children <- function(x, simplify = TRUE, namespace) {
    if (!missing(namespace))
        namespace <- match.arg(namspace)
}
```

Given a vector of GO ids, plots a GO sub-graph.


```r
##' Plots a GO subgraph with all the \code{goids} of interest
##' up to the root of the ontology.
##'
##' @title Plots a GO sub-graph
##' @param goids A \code{character} with GO ids of interest.
##' @param use One of \code{"RamiGO"} or \code{"Rgraphviz"} that
##' specifies what plot (and which format) should be generated.
##' @param showPlot A \code{logical} defining if the plot should
##' be displayes.
##' @param params A \code{list} with additional parameters to
##' be passed to other functions. See details.
##' @return This function is used for its side effects of generating
##' plots.
##' @author Laurent Gatto
##' @export
##' @examples
##' goids <-c("GO:0005739", "GO:0005773", "GO:0005783", "GO:0032588")
##' plotGO(goids)
plotGO <- function(goids,
                   use,
                   showPlot = TRUE,
                   params = list(
                       namespace="CC", edge.labels=TRUE, ## rgraphviz
                       color="#8FBDDAFF", filename="gotree", picType="svg" ## ramigo
                   )) {
    stopifnot(require("RamiGO"))
    if (missing(use))
        use <- "RamiGO"
    use <- tolower(use)
    use <- match.arg(use,tolower(c("RamiGO","Rgraphviz")))
    if (use == "rgraphviz") {
        require(Rgraphviz) || stop("Rgraphviz is required to plot GO graphs.")
        require(topGO) || stop("topGO is required to plot GO graphs.")
        gr <- topGO:::buildGOgraph.topology(goids, params$namespace)
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
        res <- Rgraphviz::agopen(graph=gr,
                                 name="graph",
                                 attrs=graphAttrs,
                                 nodeAttrs=graph::nodeRenderInfo(gr),
                                 edgeAttrs=graph::edgeRenderInfo(gr))
        if (showPlot)
            plot(res)
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
```


