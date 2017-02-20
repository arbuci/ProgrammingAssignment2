## makeCacheMatrix will create a list containing functions suitable for
## storing calculations in an environment accessible by other functions.
## The object returned can then be evaluated by cacheSolve, which will 
## first check for the stored inverse and, if not available, will calculate
## and then store based on calls to the functions defined in the object
## returned by makeCacheMatrix.

## Test case: 
## test <- matrix(c(4,2,7,6), 2, 2)
## mx <- makeCacheMatrix(test)
## cacheSolve(mx) should return 2x2 0.6, -0.2, -0.7, 0.4
## cacheSolve(mx) should return the "getting cached data" string, followed
## by same 2x2 0.6, -0.2, -0.7, 0.4

## Create functions for caching and retrieving cached values for a matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Retrieves cached inverse matrix if available, else calculates inverse
## and stores it based on setinverse function in makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
