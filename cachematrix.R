## Put comments here that give an overall description of what your functions do:
## This R script contains 2 functions. makeCacheMatrix() creates an object
## that contains a matrix and its cached inverse if available. cacheSolve()
## calculate the inverse of the matrix of a given object created by
## makeCacheMatrix() if not availabe and then store the value in the object.


## Write a short comment describing this function: 
## makeCacheMatrix take a matrix object as an argument. It returns a list of 4 
## elements, of which each is a function. The getMatrix function returns the 
## matrix object, and the setMatrix function sets/re-sets the matrix object. The
## getInverse fnction returns the inverse of the matrix object or NULL if it's
## not available. The setInverse function sets/re-sets the inverse of the matrix
## object.

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


## Write a short comment describing this function:
## cacheSolve takes a special object created by makeCacheMatrix() as the
## argument. It returns the cached inverse from the special object if it's
## availabe. Otherwise, it calculates the inverse, store the result in the
## special object and then return the inverse matrix.

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
