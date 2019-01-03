library(LazyMatrix)
context("Lazy-load matrix from file")

# Prepare test data
ex = LazyMatrix::example.matrix
tmp.file.name = tempfile(pattern = "example.matrix", fileext = ".tmp")[1]
save(ex, file = tmp.file.name)

ex.disk = lazyMatrix(file = tmp.file.name)

test_that("instantiating the LazyMatrix reproduces the original version", {
  expect_equal(ex, instantiate(ex.disk))
})
