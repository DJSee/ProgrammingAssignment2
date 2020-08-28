## makeCacheMatrix creates an object that preserves the state of the inverse matrix 
## cacheSolve creates the inverse of matrix in an efficient way so that if the inverse already 
## exists, it is not re-calculated

## Function allows you to cache the inverse of a matrix so that it is accessible 
## to the cacheSolve function and doesn't need to be re-calculated

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv<<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv<<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Function allows you to access the inverse of a matrix
## without recalculating if it has already been calculated

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  
  ## Return a matrix that is the inverse of "mat"
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
  
}


