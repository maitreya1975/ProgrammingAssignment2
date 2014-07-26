## Solution to Assignment 2:
## CachedMatrix that caches the inverse of the matrix.

## The makeCacheMatrix creates a list of functions given a matrix x.
## The list of functions has:
## set(matrix) - set the value of the matrix
## get() - returns the current value of the matrix
## setInverse(inv) - sets the inverse to inv and caches it for future invocations of getInverse()
## getInverse() - returns the cached value of the inverse
makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  matrix <- x
  
  # TODO: without << function!
  set <- function(m) {
    matrix <<- m
    cachedInverse <<- NULL
  }
  
  get <- function() matrix
  setInverse <- function(inv) {
    cachedInverse <<- inv
  }
  
  getInverse <- function() cachedInverse
  
  list(set = set, 
       get = get, 
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve returns the inverse of a cacheMatrix. It uses the cached value if present 
## or generates the inverse using solve(x) and caches the result.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv = x$getInverse()
  if(is.null(inv)) {
    inv = solve(x$get())
    x$setInverse(inv)
  } else {
    message("using cached value of matrix inverse")
  }
  inv
}
