test_that("validGO function", {
    expect_true(validGO("GO:0000014"))
    expect_false(validGO("GO:0000014", "CC"))
    expect_true(validGO("GO:0000014", "MF"))
    expect_true(validGO("GO:0000014", c("CC", "MF")))
    expect_false(validGO("GO:0000000"))
    expect_identical(validGO(c("GO:0000014", "GO:0000015")),
                     c('GO:0000014' = TRUE, 'GO:0000015' = TRUE))
    expect_identical(validGO(c("GO:0000014", "GO:0000015"), "CC"),
                     c('GO:0000014' = FALSE, 'GO:0000015' = TRUE))
    expect_identical(validGO(c("GO:0000014", "GO:0000015"), "MF"),
                     c('GO:0000014' = TRUE, 'GO:0000015' = FALSE))
    expect_identical(validGO(c("GO:0000014", "GO:0000015"), c("CC", "MF")),
                     c('GO:0000014' = TRUE, 'GO:0000015' = TRUE))
    expect_identical(validGO(c("GO:0000014", "GO:0000000")),
                     c('GO:0000014' = TRUE, 'GO:0000000' = FALSE))
})
