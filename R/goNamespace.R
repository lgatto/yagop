##' This function retrieves the namespaces of GO identifiers. If one
##' can't be found, it returns \code{NA} instead.
##'
##' @title Obtain a GO term's namespace
##' @param goid A vector or GO identifiers.
##' @return A named \code{character} vector of length equal to
##'     \code{length(goid)} containing the namespaces of the
##'     \code{goid} identifiers. A the identifier can't be found,
##'     \code{NA} is returned.
##' @author Laurent Gatto
##' @examples
##' goNamespace(c("GO:2001317", "GO:0000827", "GO:0000782"))
goNamespace <- function(goid) {
    val <- validGO(goid)
    if (any(!val))
        warning("Non valid GO ids: ", paste(goid[!val], collapse = ", "))
    res <- sapply(goid, .goNamespace)
    names(res) <- goid
    res
}

.goNamespace <- function(goid) {
    x <- GOTERM[[goid]]
    if (is.null(x)) return(NA_character_)
    x@Ontology
}
