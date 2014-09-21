## This file contains two functions 
## makecacheMatrix that returns a list of functions
## set : sets a matrix get: return the set matrix
## setinverse : sets the value of the inverse matrix (set by set function) 
## getinverse : returns the inverse matrix

## cacheSolve : this function caches the inverse of the matrix
## it tests for the inverse, if it is already calculated it returns the calculated value
## if not calculated, it calculates the inverse and stores it


## makeCacheMatrix : returns a list of functions for the matrix 
## See the comemnts at top for more information

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    
    m <<- NULL
    
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve: Does lazy loading for the matrix inverse
## Please see comments at the top for more information

cacheSolve <- function(x, ...) {
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

##  Test case
## mat <-makeCacheMatrix()
## thedata = c( 4,3,3,2)
## mat2=matrix(thedata,2)
## mat$set(mat2)
## mat$get()
## cacheSolve(mat)
## This should return 
##   [,1] [,2]
## [1,]   -2    3
## [2,]    3   -4
