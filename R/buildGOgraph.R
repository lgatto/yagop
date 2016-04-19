## Copied and adapted from topGO:::buildGOgraph.topology
buildGOgraph <- function (knownNodes, ns, where = yagopEnv) {
    nodeLookUp <- new.env(hash = TRUE, parent = emptyenv())
    isNodeInDAG <- function(node) {
        return(exists(node, envir = nodeLookUp,
                      mode = "logical", inherits = FALSE))
    }
    setNodeInDAG <- function(node) {
        assign(node, TRUE, envir = nodeLookUp)
    }
    GOParents <- get(paste("GO", ns, "PARENTS", sep = ""))
    GENE.ONTO.ROOT <- as.character(revmap(GOParents)$all)
    adjLookUP <- as.list(GOParents)
    edgeEnv <- new.env(hash = TRUE, parent = emptyenv())
    envAddEdge <- function(u, v, type) {
        assign(v, switch(type, isa = 0, partof = 1, -1),
               envir = get(u, envir = edgeEnv))
    }
    buildInducedGraph <- function(node) {
        if (isNodeInDAG(node)) 
            return(1)
        setNodeInDAG(node)
        assign(node, new.env(hash = TRUE, parent = emptyenv()),
               envir = edgeEnv)
        if (node == GENE.ONTO.ROOT) 
            return(2)
        adjNodes <- adjLookUP[[node]]
        if (length(adjNodes) == 0) 
            message("\n There are no adj nodes for node: ", node)
        for (i in 1:length(adjNodes)) {
            x <- as.character(adjNodes[i])
            envAddEdge(node, x, names(adjNodes[i]))
            buildInducedGraph(x)
        }
        return(0)
    }
    lapply(knownNodes, buildInducedGraph)
    .graphNodes <- ls(edgeEnv)
    .edgeList <- eapply(edgeEnv, function(adjEnv) {
        aux <- as.list(adjEnv)
        return(list(edges = match(names(aux), .graphNodes),
                    weights = as.numeric(aux)))
    })
    GOgraph.topo <- new("graphNEL", nodes = .graphNodes,
                        edgeL = .edgeList, edgemode = "directed")
    return(GOgraph.topo)
}
