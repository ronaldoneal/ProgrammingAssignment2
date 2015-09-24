## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than compute it repeatedly 
## Write a pair of functions that cache the inverse of a matrix.

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
 ## Functions sampled from the notes example

 ## set and get the value of the matrix

  invmat <- NULL
  
  setmat <- function(y) {
          x <<- y
          invmat <<- NULL
  }
  
  getmat <- function() x
  
  ####  set and get Inverse Matrix  ####
  
  setinv <- function(inverse) invmat <<- inverse
  
  getinv <- function() invmat
  
  list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)

}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  invmat <- x$getinv()
  
  if(!is.null(invmat)) {
    message("getting cached data")
    return(invmat)
  }
  
  data <- x$getmat()
  
  invmat <- solve(data, ...)
  
  x$setinv(invmat)
  
  invmat
}

