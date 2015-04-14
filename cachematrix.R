## These functions compute and cache the inverse of square matrices.

## Creates an invertible matrix object from the specified square matrix.
## Arguments:
##    x  a square matrix.
## Returns:
##    An invertible matrix object.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    if(!identical(x,y))
    {
      x <<- y
      m <<- NULL
    }
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Computes and caches the inverse of the specified square matrix.
## If the inverse matrix is already in cache it is returned immediately.
## Otherwise, the inverse matrix is computed, cached and then returned.
## Arguments:
##    x   an invertible matrix object (which can be created using makeCacheMatrix)
##    ... further arguments to pass to the underlying solve function call
## Returns:
##    A matrix (inverse matrix).
cacheSolve <- function(x, ...) {
  
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}

## --------------
## Sample output:
## --------------
##
## > cacheMatrix <- makeCacheMatrix(matrix(1:4,2,2))
## > cacheSolve(cacheMatrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(cacheMatrix)
## getting cached data
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheMatrix$set(matrix(4:1,2,2))
## > cacheSolve(cacheMatrix)
##      [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2
## > cacheSolve(cacheMatrix)
## getting cached data
##      [,1] [,2]
## [1,] -0.5    1
## [2,]  1.5   -2