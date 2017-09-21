## These functions allow the caching of the inverse of a matrix.
##
## makeCacheMatrix 

## This function converts a matrix into a special list of four functions
## that can store and retrieve the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
  x <<- y
  m <<- NULL
  }
  get <- function() {
    x
  }
  setsolve <- function(solve)  {
    m <<- solve
  } 
  getsolve <- function() { m
  } 
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## makeCacheSolve
##
## This function  checks if a matrix (in the special list format above)
## is in the cache, if it is, it is retrieved and output,
## if it isn't it is still output but it is also cached for next time

cacheSolve <- function(x, ...) {

        
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
