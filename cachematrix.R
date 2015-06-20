## This is a wrapper for matrix to allow caching matrix inversion next to matrix itself

## Constructor for the enchanced matrix
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL

  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }

  get <- function() x

  setInverse <- function(inverseArg) inverse <<- inverseArg
  getInverse <- function() inverse

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Inverse function that use cached value if available
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
}
