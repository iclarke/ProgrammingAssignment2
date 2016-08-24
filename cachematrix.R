## Put comments here that give an overall description of what your
## functions do

# This function creates a special "matrix" object which can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
  
}


## This fucntion returns the inverse of the matrix created by the makeCacheMatrix above : 
## if the inverse has already been calculated then it is retrieved from the cache, otherwise
## the funtion calculates it.

cacheSolve <- function(x, ...) {
        
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setinv(inv)
  inv
}
