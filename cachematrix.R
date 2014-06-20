## MakeCacheMatrix:  Constructs four methods
## Cachesolve:  Computes inverse matrix

## MakeCacheMatrix:  Methods construction function
##  (1) set: matrix initialization 
##  (2) setMatrix : store inverse matrix   
##  (3) get : retrieve the input matrix   
##  (4) getMatrix  :  retrieve cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  Set <- function(y) {
    x   <<- y
    inv <<- NULL
  }
  SetMatrix <- function(m)
    inv <<- m
  
  Get <- function()
    x
  GetMatrix <- function()
    inv
  
  ####    return an object which can use those 4 internal functions
  list(set = Set,
       get = Get,
       setmatrix = SetMatrix,
       getmatrix = GetMatrix)
}


##  cacheSolve:  Matrix inversion function.  Optimized with cache-handling functions.

cacheSolve <- function(x, ...) {
  m <- x$getmatrix()
  if (is.null(m)) {
    data <- x$get()
    m    <- solve(data, ...)
    x$setmatrix(m)
  } else {  ##  [optional] tell learner about source of returned value
    message('using cached matrix value')
  }
  m
        ## Return a matrix that is the inverse of 'x'
}
