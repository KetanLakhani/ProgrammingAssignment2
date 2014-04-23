## This code is divided into two functions - makeCacheMatrix and cacheSolve. 
## The first function creates a special matrix object that can cache its inverse
## The second computes the inverse of the special "matrix" returned by makeCacheMatrix above
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## will retrive the inverse from its cache

## makeCacheMatrix takes an input matrix and 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) m <<- solve
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

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
