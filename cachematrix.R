## The function 'makeCacheMatrix' creates a special vector which contains a list of functions: 
## 1. 'setMat' function replaces the previous matrix with the new matrix
## 2. 'getMat' function returns the matrix
## 3. 'setinverse' function sets the 'inverse'm' value to the comuted inverse 
## 4. 'getinverse' function simply renders the value of 'm'

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  setMat <- function(y) {
    x <<- y
    m <<- NULL
  }
  getMat <- function() x
  setinverse <- function(inv) m <<- inv
  getinverse <- function() m
  list(setMat = setMat, getMat = getMat,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function 'cacheSolve' computes the inverse of a matrix
## If the matrix is unchanged and inverse of the matrix is already computed, 
## it fetches the inverse from the cached data rather than recomputing it


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$getMat()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}