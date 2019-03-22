## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## uses x which is always "a square invertible matrix" and then   
  ## returns a list that sets and gets the matrix and the inverse
  
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) inv <<- inverse
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## by using what was produced in makeCacheMatrix()
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  matrixData <- x$get()
  inv <- solve(matrixData, ...)
  x$setInv(inv)
  inv
  
}
