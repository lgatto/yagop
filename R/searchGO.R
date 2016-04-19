##' This function searches the the Gene Ontology terms for a specific
##' pattern. If run in interactive mode and if the pattern matches
##' more then one term, the user is presented in a menu to choose one
##' term.
##'
##' This function searches the identifier, term and synonym fields of
##' all Gene Ontology or only a subset of the possible namespaces. The
##' pattern is cases insensitive, unless otherwise stated, and more
##' pattern matching can be further parametrised by passing additional
##' arguments to the underlying \code{\link{grep}} function.
##'
##' If more than one match is found and the function is used
##' interactively, the user is presented with a menu to select a
##' single term. If the users exits the menu without any selection (by
##' choosing 0 or cancel), all matching identifiers are returned, as
##' if the search was performed in non-interactive mode.
##'
##' If not match is found, \code{NULL} is returned invisibly with a
##' message.
##' 
##' @title Search GO terms for a matching pattern
##' @param pattern A \code{character} of length 1 (subsequent elements
##'     are ignored with a warning) to search for in GO.
##' @param namespace One or multiple GO namespaces to search for
##'     \code{pattern}. Must match \code{"CC"}, \code{"MF"} and
##'     \code{"BP"}.
##' @param ignore.case Should case be ignored. Default is \code{TRUE}.
##' @param interactive If \code{TRUE} (default) and several GO terms
##'     matched \code{pattern}, a menu is provided to the user to
##'     select one term.
##' @param graphics Should a graphical menu be used. Defaults to
##'     \code{getOption("menu.graphics")}.
##' @param ... Additional parameters, except \code{ignore.case} (see
##'     above), passed to \code{\link{grep}}.
##' @return A \code{character} with the identifiers of matched GO
##'     terms of invisibly \code{NULL} if no matches were found.
##' @author Laurent Gatto
##' @examples
##' i <- interactive()
##' searchGO("Golgi", interactive = i)
##' searchGO("Golgi apparatus", interactive = i)
##' searchGO("Golgi apparatus", namespace = "CC", interactive = i)
##' searchGO("Golgi apparatus", namespace = "BP", interactive = i)
##' searchGO("Golgi apparatus", namespace = "MF", interactive = i)
searchGO <- function(pattern,
                     namespace = c("CC", "BP", "MF"),
                     ignore.case = TRUE,
                     interactive = TRUE,
                     graphics = getOption("menu.graphics"),
                     ...) {
    if (length(pattern) > 1) {
        warning("Using only first pattern.")
        pattern <- pattern[1]
    }
    ans <- GOTerms()
    iid <- grep(pattern, GOID(ans), ignore.case = ignore.case, ...)
    itrm <- grep(pattern, Term(ans), ignore.case = ignore.case, ...)
    isyn <- grep(pattern, Synonym(ans), ignore.case = ignore.case, ...)
    ans <- ans[unique(c(iid, itrm, isyn))]
    namespace <- match.arg(namespace, several.ok = TRUE)
    ans <- ans[Ontology(ans) %in% namespace]
    if (length(ans) == 0) {
        message("No matching GO entry found.")
        return(invisible(NULL))
    } else {
        if (interactive & length(ans) > 1) {
            menuitems <- paste0(GOID(ans), " - ", Term(ans),
                                " (", Ontology(ans) , ")")
            i <- menu(menuitems, graphics = graphics,
                      title = "Select a GO term")
            if (i != 0) ans <- ans[i] ## 0 to exit from menu
        }
    }
    ans <- GOID(ans)
    names(ans) <- NULL
    ans
}
