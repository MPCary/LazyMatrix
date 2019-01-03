################################
### Create generic functions ###
################################

#' Instantiate a reference object
#'
#' @description This is a generic function used to retrieve data from a
#' reference object (such as a \code{\link[LazyMatrix]{LazyMatrix-class}}) in
#' its base form (e.g., a \code{\link[base]{matrix}}).
#'
#' @param object Object to instantiate.
#'
#' @seealso \code{\link[LazyMatrix]{cache}}
#'
#' @examples
#' x = matrix(rnorm(1000 * 100), 1000, 100)
#' temp = tempfile(pattern = "example.matrix", fileext = ".tmp")
#' save(x, file = temp)
#' y = lazyMatrix(file = temp)
#'
#' dim(y@.Data) # 1 x 1
#' z = instantiate(y)
#' dim(z@.Data) # 1000 x 100
#' class(z) # "matrix"
#'
setGeneric("instantiate", def = function(object) {
  object@.Data
})

setGeneric("as.LazyMatrix", def = function(x) {
  standardGeneric("as.LazyMatrix")
})

#' Cache a reference object
#'
#' @description This is a generic function used to retrieve data from a
#' reference object (such as a \code{\link[LazyMatrix]{LazyMatrix-class}}),
#' but the returned object remains a member of the reference class rather
#' than the base class.
#'
#' @param object Object to cache.
#'
#' @seealso \code{\link[LazyMatrix]{instantiate}}
#'
#' @examples
#' x = matrix(rnorm(1000 * 100), 1000, 100)
#' temp = tempfile(pattern = "example.matrix", fileext = ".tmp")
#' save(x, file = temp)
#' y = lazyMatrix(file = temp)
#'
#' dim(y@.Data) # 1 x 1
#' z = cache(y)
#' dim(z@.Data) # 1000 x 100
#' class(z) # "LazyMatrix"
#'
setGeneric("cache", def = function(object) {
  object
})
