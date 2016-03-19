## Put comments here that give an overall description of what your
## This functio makeCacheMatrix() creates a special "Vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the Matrix
## get the value of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## This function calculates the Inverse of the special "vector" created with the above function. 
## It first checks to see if the Inverse has already been calculated. If so, it gets the Inverse from the cache and skips the computation. 
## Otherwise, it calculates the Inverse of the Matrix and sets the value of the Inverse in the cache via the setmean function.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  ## Return a matrix that is the inverse of 'x'
  inv 
}
