## The program enables you to compute the inverse of a matrix 
## with efficient computation time

## This function creates a matrix object that can cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  
  set <- function(y) {
    x <<- y
    x_inv <<- NULL
  }
  get <- function() x
  
  setinv <- function(inv) x_inv <<- inv
  getinv <- function() x_inv
  
  list(set=set, get=get, setinv= setinv, getinv=getinv)
}

## This function computes the inverse of the matrix stored by makeCacheMatrix. If the inverse
## has been already computed cacheSolve retrive it from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x_inv <- x$getinv()
  if(!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  data <- x$get()
  x_inv <- solve(data, ...)
  x$setinv(x_inv)
  x_inv
}
