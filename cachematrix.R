## This code is divided into two functions - makeCacheMatrix and cacheSolve. 
## The first function creates a special matrix object that can cache its inverse
## The second computes the inverse of the special "matrix" returned by makeCacheMatrix above
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
## will retrive the inverse from its cache

## makeCacheMatrix is a function that contains the plan for caching a matrix

makeCacheMatrix <- function(x = matrix()) {
  
## sets the initial variable m as null
  m <- NULL

## this function uses the superassign to assign x and m to 
## an environment one level up
## and this will feed the get function
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
## this fucntion does not take any arguments and returns x when called
  get <- function() x
  
  # This takes the instance of the matrix and assigns to m
  setInverse <- function(Invmatrix) m <<- Invmatrix
  
  # This assigns the instance of the inverse matrix to m
  getInverse <- function() m

## This creates a list of the properties that we are interested in namely
## set, get, setInverse and getInverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve is a function that looks for the inverse of the matrix in cache
## If the informaiton is found in cache, then it will return the cached information
## with a messeage cached data
## if the information os not found in cache, then it will calcualte the inverse
## using solve() and return the inverse of the matrix  as m

cacheSolve <- function(x, ...) {
## This checks whether m is in cache, if it is then it will get cached data and 
## return m, otherwise it will calcualte the inverse and return that as m
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
