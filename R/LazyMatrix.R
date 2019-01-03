################################
### Create class definitions ###
################################


# Required to allow package and file.path to be either character or NULL:
setClassUnion("characterOrNULL", c("character", "NULL"))

#' An S4 class for lazy loading matrices
#'
#'@description The LazyMatrix class extends \code{\link[base]{matrix}} by providing slots
#'that define a reference to a data matrix stored in a package or file.  Methods are
#'provided for most base functions to allow LazyMatrix objects to be used in place
#'of regular matrices; the file or package data is loaded on-the-fly when needed.  For other
#'applications (e.g., user-defined functions that expect a \code{matrix}), the
#'\code{\link[LazyMatrix]{instantiate}} method can be used to create a regular \code{matrix}
#'from a LazyMatrix.
#'
#'@usage ## Constructor:
#'lazyMatrix(..., name = NULL, package = NULL, file = NULL)
#'
#'@slot ... Optional arguments to be passed to \code{\link[base]{matrix}}
#'@slot name If the desired data resides in a package, the character string name of the data object
#'@slot package If the desired data resides in a package, the character string name of the package
#'@slot file If the desired data resides in a file, the character string full path of the file
#'
#'@details LazyMatrix is a subclass of \code{\link[base]{matrix}} with three additional slots.
#'If these additional slots (name, package, and file) are NULL, a LazyMatrix behaves
#'as a regular matrix.  If either file or both name and package are not NULL, however, a LazyMatrix
#'behaves as if it contains the data in those locations (and ignores whatever is actually
#'stored in its @.Data slot, typically a 1 x 1 matrix with a value of NA).
#'
#'@examples
#'x = matrix(rnorm(1000 * 100), 1000, 100)
#'temp = tempfile(pattern = "example.matrix", fileext = ".tmp")
#'save(x, file = temp)
#'y = lazyMatrix(file = temp)
#'
#'## Check dimensions
#'dim(x) # 1000 x 100
#'dim(y) # 1000 x 100
#'
#'## Check object sizes
#'object.size(x) # ~800 KB
#'object.size(y) # ~1.5 KB
#'
#'@include GenericMethods.R
#'
lazyMatrix = setClass("LazyMatrix",
                      slots = c("name" = "characterOrNULL",
                                "package" = "characterOrNULL",
                                "file" = "characterOrNULL"),
                      contains = c("matrix")
)

setMethod(f= "initialize", signature = "LazyMatrix",
          function(.Object, ..., name = NULL, package = NULL, file = NULL) {
            .Object@name = name
            .Object@package = package
            .Object@file = file

            # Create a typical matrix
            .Object@.Data = matrix(...)@.Data

            # Check validity
            validObject(.Object)

            # Store the data if cache = TRUE
            # if(cache) .Object@.Data = instantiate(.Object)
            ## No - cache function only makes sense to cache after first use

            # Return .Object
            .Object
})

validLazyMatrix = function(object) {
  if(!is.null(object@package) & is.null(object@name)) {
    return("package specified without data object name.")
  }

  # Attempt to load data
  if(identical(object@.Data, matrix())) {
    # Load data
    if(!is.null(object@package)) {
      # Load from package
      d = try(do.call("::", list(object@package, object@name)))
      if(class(d) == "try-error") return(FALSE) else object@.Data = d
    } else if(!is.null(object@file)) {
      # Load from file
      object@.Data = try(local(get(load(file.path(object@file)))))
    } else {
      # The matrix is empty with nothing to load
    }
  }

  # Check what was loaded
  test = class(object@.Data)
  if(!(test == "matrix")) return("Loaded data must be a matrix.")

  TRUE
}

setValidity("LazyMatrix", method = validLazyMatrix)

############################
### Create class methods ###
############################

setMethod(f = "instantiate", signature = "LazyMatrix",
          function(object) {
            if(identical(object@.Data, matrix())) {
              # Attempt to load data
              if(!is.null(object@package)) {
                # Load from package
                object@.Data = do.call("::", list(object@package, object@name))
              } else if(!is.null(object@file)) {
                # Load from file
                object@.Data = local(get(load(file.path(object@file))))
              } else {
                # The matrix is empty
              }
            }
            return(object@.Data)
          }
)

setMethod(f = "cache", signature = "LazyMatrix",
          function(object) {
            object@.Data = instantiate(object)
            return(object)
          }
)

setMethod(f = "show", signature = "LazyMatrix",
          function(object) {
            show(instantiate(object))
})

## Operation methods:

setMethod(f = "Ops", signature = c(e1 = "LazyMatrix", e2 = "numeric"),
          function(e1, e2) {
            e1.i = instantiate(e1)
            e2.i = instantiate(e2)
            callGeneric(e1.i, e2.i)
          })

setMethod(f = "Ops", signature = c(e1 = "numeric", e2 = "LazyMatrix"),
          function(e1, e2) {
            message("Testing...")
            e1.i = instantiate(e1)
            e2.i = instantiate(e2)
            callGeneric(e1.i, e2.i)
          })

setMethod(f = "Ops", signature = c(e1 = "LazyMatrix", e2 = "LazyMatrix"),
          function(e1, e2) {
            e1.i = instantiate(e1)
            e2.i = instantiate(e2)
            callGeneric(e1.i, e2.i)
          })

setMethod(f = "Math", signature = "LazyMatrix",
          function(x) {
            message("Testing...")
            x.i = instantiate(x)
            callGeneric(x.i)
          })

setMethod(f = "Math2", signature = c(x = "LazyMatrix"),
          function(x, digits) {
            message("Testing...")
            x.i = instantiate(x)
            callGeneric(x.i, digits)
          })

setMethod(f = "Summary", signature = "LazyMatrix",
          function(x, ..., na.rm = FALSE) {
            message("Testing...")
            x.i = instantiate(x)
            callGeneric(x.i, ..., na.rm)
          })

setMethod(f = "Complex", signature = "LazyMatrix",
          function(z) {
            z.i = instantiate(z)
            callGeneric(z.i)
          })

setMethod(f = "[", signature = c(x = "LazyMatrix"),
          function(x, i, j, ..., drop = TRUE) {
            x.i = instantiate(x)
            x.i[i, j]
          })

## Size / dimension methods:

setMethod(f = "dim", signature = c(x = "LazyMatrix"),
          function(x) {
            x.i = instantiate(x)
            callGeneric(x.i)
          })

setMethod(f = "ncol", signature = "LazyMatrix",
          function(x) {
            dim(x)[2]
          })

setMethod(f = "nrow", signature = "LazyMatrix",
          function(x) {
            dim(x)[1]
          })

setMethod(f = "dimnames", signature = "LazyMatrix",
          function(x) {
            x.i = instantiate(x)
            dimnames(x.i)
          })

## Conversion methods:

setMethod(f = "as.matrix", signature = "LazyMatrix",
          function(x) {
            x.i = instantiate(x)
            as.matrix(x.i)
          })

setMethod(f = "as.LazyMatrix", signature = "ANY",
          function(x) {
            x.m = as.matrix(x)
            lazyMatrix(x.m, ncol = ncol(x.m), nrow = nrow(x.m), dimnames = dimnames(x.m))
          })

