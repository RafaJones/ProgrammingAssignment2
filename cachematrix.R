
  
## instead of constantly calculating the inverse of a matrix, I have written 
##two functions.

##The first will make our matrix x using makeCacheMatrix and 
## also calculate the inverse of matrix x using the solve function. 

## the second, will either find the cache inverse matrix 
##from the first function, 
## or if the matrix has changed, recalculate the inverse. 

## makeCacheMatrix creates a matrix x by creating a NULL m, then 
## making m equal the inverse of x using solve. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL 
  set <- function(z) {
    x <<- z
    m <<- NULL
  }
  
  get <- function() x
  matrixset <- function(solve) m <<- solve
  matrixget <- function() m
  list(set = set, get = get,
       matrixset = matrixset,
       matrixget = matrixget)
}


## CacheSolve calculates the inverse of x, unless m is NOT NULL
## if m is NOT NULL it will state "getting cached data" and simply return m
##this saves memory and time as m will be retrieved from makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$matrixget()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$matrixset(m)
    m
}

