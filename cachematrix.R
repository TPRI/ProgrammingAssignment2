## A pair of functions that cache the inverse of a matrix.

## Creates a special "matrix" object that has set, get "methods" 
## and setinverse and getinverse "methods" that allow the inverse to be cached.  

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(mean) inv <<- inverse
  getinverse <- function() inv
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the matrix in the makeCacheMatrix object.
## If inverse has already been cached, returns inverse from cache.

cacheSolve <- function(x, ...) {
  
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  
  inv
}
