library(LazyMatrix)
context("LazyMatrix mathematical operations")

# Prepare test data
ex = instantiate(LazyMatrix::example.matrix)
tmp.file.name = tempfile(pattern = "example.matrix", fileext = ".tmp")[1]
save(ex, file = tmp.file.name)
ex.disk = lazyMatrix(file = tmp.file.name)
ex.package = lazyMatrix(name = "example.matrix", package = "LazyMatrix")

# First for ex.disk
test_that("simple arithmetic works as expected", {
  expect_equal(ex + 123, ex.disk + 123)
  expect_equal(ex - 123, ex.disk - 123)
  expect_equal(ex * 123, ex.disk * 123)
  expect_equal(ex / 123, ex.disk / 123)

})

test_that("comparisons work as expected", {
  expect_equal(ex == 0, ex.disk == 0)
  expect_equal(ex > 0, ex.disk > 0)
  expect_equal(ex != 0, ex.disk != 0)
  expect_equal(ex >= 0, ex.disk >= 0)
  expect_equal(ex <= 0, ex.disk <= 0)
})

test_that("math functions work as expected", {
  expect_equal(abs(ex), abs(ex.disk))
  expect_equal(floor(ex), floor(ex.disk))
  expect_equal(round(ex, 2), round(ex.disk, 2))
})

test_that("math summary functions work as expected", {
  expect_equal(max(ex), max(ex.disk))
  expect_equal(range(ex), range(ex.disk))
  expect_equal(sum(ex, 2), sum(ex.disk, 2))
})

# Now test for ex.package
test_that("simple arithmetic works as expected", {
  expect_equal(ex + 123, ex.package + 123)
  expect_equal(ex - 123, ex.package - 123)
  expect_equal(ex * 123, ex.package * 123)
  expect_equal(ex / 123, ex.package / 123)

})

test_that("comparisons work as expected", {
  expect_equal(ex == 0, ex.package == 0)
  expect_equal(ex > 0, ex.package > 0)
  expect_equal(ex != 0, ex.package != 0)
  expect_equal(ex >= 0, ex.package >= 0)
  expect_equal(ex <= 0, ex.package <= 0)
})

test_that("math functions work as expected", {
  expect_equal(abs(ex), abs(ex.package))
  expect_equal(floor(ex), floor(ex.package))
  expect_equal(round(ex, 2), round(ex.package, 2))
})

test_that("math summary functions work as expected", {
  expect_equal(max(ex), max(ex.package))
  expect_equal(range(ex), range(ex.package))
  expect_equal(sum(ex, 2), sum(ex.package, 2))
})
