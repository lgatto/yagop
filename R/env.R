yagopEnv <- new.env(parent = emptyenv())

sql <- "SELECT go_id FROM go_term WHERE ontology IN"
for (ns in c("BP", "MF", "CC")) {
    ids <- dbGetQuery(GO_dbconn(),
                      paste(sql, "('", ns, "');", sep = ""))$go_id
    assign(paste0("GO", ns, "TERM"), GOTERM[ids], yagopEnv)
}
