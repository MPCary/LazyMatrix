library(LazyMatrix)
context("Convert to/from LazyMatrices")

# Prepare test data
ex = LazyMatrix::example.matrix
tmp.file.name = tempfile(pattern = "example.matrix", fileext = ".tmp")[1]
save(ex, file = tmp.file.name)
ex.disk = lazyMatrix(file = tmp.file.name)
ex.package = lazyMatrix(name = "example.matrix", package = "LazyMatrix")

test_that("creating a LazyMatrix then converting it to a matrix produces the original data", {
  expect_identical(ex, as.matrix(lazyMatrix(ex, ncol = ncol(ex), nrow = nrow(ex),
                                            dimnames = dimnames(ex))))
  expect_identical(data.frame(ex, check.names = FALSE),
                   as.data.frame(lazyMatrix(ex, ncol = ncol(ex), nrow = nrow(ex),
                                            dimnames = dimnames(ex))))
  expect_identical(c(ex), as.vector(lazyMatrix(ex, ncol = ncol(ex), nrow = nrow(ex),
                                               dimnames = dimnames(ex))))
})
