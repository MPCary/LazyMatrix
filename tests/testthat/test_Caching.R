library(LazyMatrix)
context("Test cache method")

# Prepare test data
ex = LazyMatrix::example.matrix
tmp.file.name = tempfile(pattern = "example.matrix", fileext = ".tmp")[1]
save(ex, file = tmp.file.name)

ex.disk = lazyMatrix(file = tmp.file.name)
ex.disk = cache(ex.disk)

test_that("caching a LazyMatrix properly stores its data", {
  expect_equal(ex, ex.disk@.Data)
})
