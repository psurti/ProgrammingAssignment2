## Author: P. Surti - R Programming Assignment 3 
##
## This function computes the inverse of the special
## matrix returned by makeCacheMatrix call. If the
## inverse has already been calculated ( and the
## matrix has not changed), then the cacheSolve
## method returns the cached inverse. Since matrix
## inversion is a costly computation it is cached so
## that repeated calls to access the inverted matrix
## does not get re-computed again.
## Usage: 
##     m <- makeCacheMatrix(matrix(1:4,2,2))
##	   m$get() - returns the input matrix
##     cacheSolve(m) - calculates inverted matrix and caches it
##     cacheSolve(m) - Another call returns the cached inverted matrix

## Creates a special matrix object that is
## used to cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {
  # Set some values
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  # Set getter/setter methods
  get <- function() x
  setInverse <- function(solve) m<<-solve
  getInverse <- function() m
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
}


## Calculates and caches the inverse of a given
## matrix
##
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  ## Check if cached
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m<-solve(data,...)
  x$setInverse(m)
  m
}
