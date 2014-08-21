## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  
  setMatrix <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  getMatrix <- function() x
  
  setInverse <- function(i) inverse <<- i
  
  getInverse <- function() inverse
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
  
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  if (!is.null(inv)) {
    
    message ("getting cached data")
    return (inv)
    
  }
  
  mat <- x$getMatrix()
  inv <- solve(mat)
  x$setInverse(inv)
  inv
}
