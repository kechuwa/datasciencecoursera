## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#This function creates a matric that can cache the inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  #matrix setter
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  #matrix getter
  get <- function() {
    x
  }
  
  #set inverse of the matrix
  setInverse <- function(inverse) {
    inv <<- inverse
  }
  #return the inverse of the matrix set above
  getInverse <- function() {
    inv
  }
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## Write a short comment describing this function
#This function returns cached inversed matrix if available in cache
#If the matrix not available in cache, computes inverse and sets in cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inv <- x$getInverse()
  
  #return from cache if exists
  if (!is.null(inv)) {
    return(inv)
  }
  
  mtr <- x$get()
  inv <- solve(mtr, ...)
  
  #set in cache
  x$setInverse(inv)
  
  inv
}
