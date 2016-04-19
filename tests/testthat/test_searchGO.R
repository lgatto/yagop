test_that("searchGO function", {
    i <- FALSE
    xcc <- x <- searchGO("Golgi apparatus", "CC", interactive = i)
    expect_length(x, 3)
    x <- searchGO("^Golgi apparatus$", "CC", interactive = i) ## exact matching
    expect_identical(x, "GO:0005794")
    xmf <- searchGO("Golgi apparatus", "MF", interactive = i)
    expect_null(xmf)
    xbp <- searchGO("Golgi apparatus", "BP", interactive = i)
    expect_length(xbp, 8)
    x <- searchGO("Golgi apparatus", interactive = i)
    expect_identical(sort(x), sort(c(xcc, xmf, xbp)))
    expect_warning(x2 <- searchGO(c("Golgi apparatus", "ignored"), interactive = i))
    expect_identical(x, x2)
})
