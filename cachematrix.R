## Provides capability of caching the inverse of a matrix
##
## Note: Makes use of the <<- method which searches up through the parent environments to find a variable
## to set. This is different to <- which sets a variable in the current environment

## Returns a 'matrix' object, that is a list that wraps a matrix, providing four functions:
## Contains three functions that:
##  1. Get the matrix
##  2. Set the matrix
##  3. Get the inverse of the matrix
##  4. Set the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  ##Function definitions
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  
  ##Create object (a list) containing these function definitions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function will retrieve a cached inverse from the 'matrix' object if available, otherwise
## will invert the matrix, and cache and return the result
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    ## Return the cached inverse
    return(i)
  }
  ## retrieve the raw matrix and solve
  data <- x$get()
  i <- solve(data, ...)
  ## Cache the solved matrix and return
  x$setInverse(i)
  i
}

