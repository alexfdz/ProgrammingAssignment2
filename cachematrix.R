# Functions that cache the inverse of a matrix
# 
# Usage:
#   matrix <- matrix(c(3,1,2,1),nrow=2,ncol=2)
#   chachedMatrix <- makeCacheMatrix(matrix) ## Create the cacheable object
#   
#   cacheSolve(chachedMatrix) ## Calculates the inverse in the first execuption
#   cacheSolve(chachedMatrix) ## Following executions wil just return the cached value

## This function creates a special "matrix" object that can cache its inverse.
##
## matrix: The incoming matrix object to wrap
## Returns  matrix wrapped object with methods:
##    get:  sets the value of the Matrix
##    set:  gets the value of the Matrix
##    setInverse:   sets the cached value of the Inverse of matrix
##    getInverse:   gets the cached value of the Inverse of matrix
makeCacheMatrix <- function(matrix = matrix()) {
  inverse <- NULL
  set <- function(newMatrix) {
    matrix <<- newMatrix
    inverse <<- NULL
  }
  get <- function() matrix
  setInverse <- function(newInverse) inverse <<- newInverse
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already
# been calculated (and the matrix has not changed), then 
# the cachesolve should retrieve the inverse from the cache
##
## cachedMatrix: The wrapped matrix (see #makeCacheMatrix)
## Returns Inverse of the matrix
cacheSolve <- function(cachedMatrix) {
  inverse <- cachedMatrix$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  inverse <- solve(cachedMatrix$get())
  cachedMatrix$setInverse(inverse)
  inverse
}
