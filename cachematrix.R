## These functions can be used to create an object that 
## stores a matrix and cache's its inverse

## makeCacheMatrix creates a special matrix, which contains 
## a set of functions to set and get the value of the matrix 
## and its inverse

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(value) {
    x <<- value
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(value) inverse <<- value
  getInverse <- function() inverse
  
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse);
}

## cacheSolve calculates the mean of the special matrix created
## with makeCacheMatrix. It first checks to see if the inverse 
## has already been calculated. If so, it returns the inverse 
## stored in the cache. Otherwise it caluclates and returns the
## inverse.

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    return (inverse)
  }
  matrix <- x$get()
  inverse <- solve(matrix, ...)
  x$setInverse(inverse)
  
  inverse
}
