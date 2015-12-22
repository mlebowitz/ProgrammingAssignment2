## This set of functions provides a way to create and object that can be used
## to can be used to store a matrix and cache its inverse so it only needs to
## computed one. 

## makeCacheMatrix creates an object that includes the input matrix along with
## functions that will get and set its inverse. It provides a warning if the
## matrix is not square (an hence cannot be inverted). Only a warning because
## we might want to use the construct for functions other than inverse.

makeCacheMatrix <- function(x = matrix()) {
  if(!ncol(x)==nrow(x)) {
    message("Warning -- input is not a square matric")
  }
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of an object create my makeCacheMatrix. 
## If there is an already cached value it returns it. If not it checks to 
## make sure the matrix is square, and if so it inverts it, saves the inverse
## and then returns the inverse.

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("returning cached data")
    return(m)
  }
  data <- x$get()
  if(!ncol(data)==nrow(data)) {
    message("data is not a square matric")
    return()
  }
  
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
