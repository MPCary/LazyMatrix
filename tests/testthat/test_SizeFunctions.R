library(LazyMatrix)
context("LazyMatrix size functions")

# Prepare test data
ex = LazyMatrix::example.matrix
tmp.file.name = tempfile(pattern = "example.matrix", fileext = ".tmp")[1]
save(ex, file = tmp.file.name)

ex.disk = lazyMatrix(file = tmp.file.name)
ex.package = lazyMatrix(name = "example.matrix", package = "LazyMatrix")

test_that("dim() returns the proper dimensions", {
  expect_equal(dim(ex), c(5,5))
  expect_equal(dim(ex.disk), c(5,5))
  expect_equal(dim(ex.package), c(5,5))
})

test_that("ncol() returns the proper number of columns", {
  expect_equal(ncol(ex), 5)
  expect_equal(ncol(ex.disk), 5)
  expect_equal(ncol(ex.package), 5)
})

test_that("nrow() returns the proper number of columns", {
  expect_equal(nrow(ex), 5)
  expect_equal(nrow(ex.disk), 5)
  expect_equal(nrow(ex.package), 5)
})
