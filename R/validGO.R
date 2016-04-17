##' This function checks if a GO identifier is valid by looking it up
##' in the \code{GO.db} package's \code{\link{GOTERM}} term
##' map. Depending whether a namespace is provided, the function will
##' look in one, multiple or all namespaces. 
##'
##' @title Checks the validity of GO identifiers
##' @param goid A vector of GO identifier.
##' @param namespace One of several of \code{"CC"}, \code{"MF"} or
##'     \code{"BP"}.
##' @return \code{TRUE} if \code{goid} is a valid GO identifier,
##'     \code{FALSE} otherwise.
##' @author Laurent Gatto
##' @examples
##' ## GO:0000014 is a valid MF Term
##' validGO("GO:0000014")
##' validGO("GO:0000014", "CC")
##' validGO("GO:0000014", "MF")
##' validGO("GO:0000014", c("CC", "MF"))
##' ## GO:000000 is not valid
##' validGO("GO:0000000")
##' ## GO:0000015 is a valid CC Term
##' validGO(c("GO:0000014", "GO:0000015"))
##' validGO(c("GO:0000014", "GO:0000015"), "CC")
##' validGO(c("GO:0000014", "GO:0000015"), "MF")
##' validGO(c("GO:0000014", "GO:0000015"), c("CC", "MF"))
##' validGO(c("GO:0000014", "GO:0000000"))
validGO <- function(goid, namespace) {
    sapply(goid, .validGO, namespace)
}

.validGO <- function(goid, namespace) {
    xx <- GOTERM[[goid]]
    if (is.null(xx))
        return(FALSE)
    if (!missing(namespace)) {
        namespace <- match.arg(c("CC","BP","MF"),
                               namespace,
                               several.ok = TRUE)
        if (!xx@Ontology %in% namespace)
            return(FALSE)
    }
    return(TRUE)
}
