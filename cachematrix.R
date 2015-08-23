## The function duo is caching inverse of a matrix rather than computing it repeatedly. The
## two functions are used to cache the inverse of a matrix and call it when needed.

## makeCacheMatrix returns a list of functions:
# 1. set: set the value of the matrix
# 2. get: get the value of the matrix
# 3. setinverse: set the value of inverse of the matrix
# 4. getinverse: get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  ## Return a list of functions for caching
  invr <- NULL
  set <- function(y) {
    x <<- y
    invr <<- NULL
  }
  get <- function() x
  setinverse <- function(solved) invr <<- solved
  getinverse <- function() invr
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


##  Function cacheSolve returns the inverse of the matrix. It checks for the solution in cache, and computes new
##  one only if the matrix is not yet calculated. After solving the inverse matrix, it stores in cache for later use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invr <- x$getinverse()
  if(!is.null(invr)) {
    message("getting cached data ... ")
    return(invr)
  }
  data <- x$get()
  invr <- solve(data, ...)
  x$setinverse(invr)
  return(invr)
}