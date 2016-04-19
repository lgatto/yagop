yagopEnv <- new.env(parent = emptyenv())


prepareGOTermsInEnv <- function() {
    sql <- "SELECT go_id FROM go_term WHERE ontology IN"
    for (ns in c("BP", "MF", "CC")) {
        ids <- dbGetQuery(GO_dbconn(),
                          paste(sql, "('", ns, "');", sep = ""))$go_id
        assign(paste0("GO", ns, "TERM"),
               GOTERM[ids],
               yagopEnv)
    }
}

GOCCTerms <- function() yagopEnv$GOCCTERM
GOCCList <- function() as.list(GOCCTerms())

GOMFTerms <- function() yagopEnv$GOMFTERM
GOMFList <- function() as.list(GOMFTerms())

GOBPTerms <- function() yagopEnv$GOBPTERM
GOBPList <- function() as.list(GOBPTerms())

GOTerms <- function() GO.db::GOTERM
GOList <- function() as.list(GOTerms())
