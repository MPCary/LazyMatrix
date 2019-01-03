library(LazyMatrix)
context("LazyMatrix subsetting")

# Prepare test data
ex = instantiate(LazyMatrix::example.matrix)
tmp.file.name = tempfile(pattern = "example.matrix", fileext = ".tmp")[1]
save(ex, file = tmp.file.name)

ex.disk = lazyMatrix(file = tmp.file.name)
ex.package = lazyMatrix(name = "example.matrix", package = "LazyMatrix")

test_that("[] returns the proper subset", {
  expect_equal(ex[1:3,1:3], ex.disk[1:3,1:3])
  expect_equal(ex[1:3,1:3], ex.package[1:3,1:3])
})
