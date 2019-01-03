library(LazyMatrix)
context("Lazy-load matrix from package")

# Prepare test data
ex = LazyMatrix::example.matrix
ex.package = lazyMatrix(name = "example.matrix", package = "LazyMatrix")

test_that("instantiating the LazyMatrix reproduces the original version", {
  expect_equal(ex, instantiate(ex.package))
})
