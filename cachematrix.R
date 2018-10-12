## These funcitons provide a machanism to return the inverse of a matrix.
## this is obtained from a solve function
## These functions provide a mechanism to cache return values rather than recalculation.
## 
## Example usage
##    mtx = matrix(c(2, 4, 3, 1, 5, 7), nrow=3,  ncol=2) 
##    mk<- makeCacheMatrix(mtx)
##    cacheSolve(mk)
## Function returns a list which will hold a list which caches functions and state 
## of a return value of a matrix calculation.
makeCacheMatrix <- function(x = matrix()) {
  
  ## cached value to return
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  
  setsolved <- function(solved) m <<- solved
  getsolved <- function() m
  
  list(set = set, get = get,
       setsolved = setsolved,
       getsolved = getsolved)
}


## function will return a cached solve value of a matrix solve
## if that cached version does not exist, it will call solve, and c
## Store the value.
cacheSolve <- function(x, ...) {

  m <- x$getsolved()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolved(m)
  m
}


