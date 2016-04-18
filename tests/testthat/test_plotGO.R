test_that("plotGO function", {
    gocc <-c("GO:0005739", "GO:0005773")
    gomf <- "GO:0000132"
    nogo <- "GO:0000000"

    expect_error(plotGO(nogo))
    expect_error(plotGO(gocc, use = "err"))
    expect_error(plotGO(c(gocc, gomf)))
    expect_error(plotGO(c(gocc, nogo)))

    expect_is(plotGO(gocc, showPlot = TRUE), "character")
    expect_is(plotGO(gocc, use = "Rgraphviz", showPlot = TRUE), "graphNEL")

    p1 <- plotGO(gocc, showPlot = TRUE)
    p2 <- plotGO(gocc, showPlot = FALSE)
    ## expect_equal(p1, p2)
    p1 <- plotGO(gocc, showPlot = TRUE, use = "Rg")
    p2 <- plotGO(gocc, showPlot = FALSE, use = "Rg")
    expect_equal(p1, p2)
})
