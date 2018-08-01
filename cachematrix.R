## These two functions, when used together, will take a matrix
## and return its inverse, using a cache to avoid recalculation. 

## This function creates a "special matrix" that is able to 
##cache its inverse. This "special matrix" is really a list
## containng a function to set and get the value of the input
## matrix. The output is used as the argument for second function. 

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x <<- y 
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function computes the inverse of the "special matrix" returned by
## the makeCacheMatrix function, and returns its inverse. If it
## has been already calculated, it will just return the cached 
## output. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}



