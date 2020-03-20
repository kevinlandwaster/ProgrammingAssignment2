## Cache the results of inverting a matrix.
##
## Inverting a matrix can be very time consuming, depending on the
## size of the matrix. If the inversion of a particular matrix is needed multiple
## times in, for example, a loop, it would be efficient to cache the results
## of inverting the matrix.

## makeCacheMatrix() creates a vector which contains a matrix, the results
## of inverting the matrix and a variety of function for working with the
## matrix..
##
## If the matrix changes between setting the matrix and calculating its
## inverse, the cache result will be incorrect

makeCacheMatrix <- function(m = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    m <<- y
    inverseMatrix <<- NULL
  }
  get <- function() m
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Calculate the inverse of a matrix.
##
## The results of the matrix inversion will be cached. Calling this
## function a second time will use the cached value.
##
## The matrix must be created using the makeCacheMatrix() function.
##
## As an example:
##   m <- makeCacheMatrix(matrix(1:900, nrow=30, ncol=30))
##   cacheSolve(m)
##   cacheSolve(m)
##
## The first call will be slow, the second call much faster.

cacheSolve <- function(matrixCache, ...) {
  inverse <- matrixCache$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- matrixCache$get()
  inverse <- solve(data, ...)
  matrixCache$setInverse(inverse)
  inverse
}
