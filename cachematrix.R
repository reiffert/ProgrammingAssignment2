## The following 2 functions are used to create a special object that stores an invertible matrix and caches its
## matrix inverse. These functions assume that the matrix supplied is always invertible.

## Creates a special "matrix" object that can cache its own inverse. This matrix object is really a list
## containing the following functions:
## - set the matrix. Assumes that the input matrix is invertible.
## - get the matrix
## - set the inverse of the matrix.  
## - get the inverse of the matrix
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

## Computes and returns the inverse matrix for a "matrix" object created with the makeCacheMatrix function.
## The first call to cacheSolve(x) computes and caches the inverse matrix. Subsequent calls return the cached
## inverse matrix as long as the matrix has not changed. If the matrix changes, the next call to cacheSolve(x)
## computes and caches the inverse matrix for the changed matrix, and subsequent calls return the cached value.
cacheSolve <- function(x, ...) {
    
  m <- x$getinverse()
  if (!is.null(m)) {
    message('returning cached matrix inverse')
    return(m)
  }
  message('calculating matrix inverse')
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m 
}
