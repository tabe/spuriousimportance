test_that("n for the number of rows", {
    ds <- generate_dataset(1000)
    expect_equal(nrow(ds), 1000)
})
