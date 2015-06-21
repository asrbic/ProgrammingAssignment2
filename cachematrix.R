## The functions in the R file allow the inverse
## of a matrix to be cached and reused instead of 
## having to recalculate the inverse. 

## This function return a list of getter and setter
## functions allowing access to the given array and
## its cached inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function takes the given cacheMatrix
## created by makeCacheMatrix() and calculates
## its inverse if it is not already cached, otherwise
## it will retrieve the previously calculated
## cached inverse. Any additional arguments passed
## in via the ... argument will be provided to 
## the solve(m, ...) call which does the inverse
## calculation. 

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