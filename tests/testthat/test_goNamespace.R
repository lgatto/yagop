test_that("goNamespace function", {
    expect_identical(c('GO:0005739' = "CC"),
                     goNamespace("GO:0005739"))
    expect_identical(c('GO:10101010' = NA,
                       'GO:0005783' = "CC",
                       'GO:0032588' = "CC"),
                     goNamespace(c("GO:10101010",
                                   "GO:0005783",
                                   "GO:0032588")))
    expect_warning(goNamespace(c("GO:10101010",
                                 "GO:0005783",
                                 "GO:0032588")))
    expect_identical(goNamespace(c("GO:2001317",
                                   "GO:0000827",
                                   "GO:0000782")),
                     c('GO:2001317' = "BP",
                       'GO:0000827' = "MF",
                       'GO:0000782' = "CC"))
})
