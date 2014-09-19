## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
