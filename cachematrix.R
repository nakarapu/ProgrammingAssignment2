## Put comments here that give an overall description of what your
## functions do

## Create a special matrix that can cache its inverse
## This special matrix is in fact a list of functions
## that enable the caching

makeCacheMatrix <- function(x = matrix()) {
  mat <- x
  inv <- NULL
  
  get <- function() mat
  set <- function(a) {
    # check if the matrix is same as before
    if(!identical(mat,a)) {
      mat <<- a
      inv <<- NULL
    }
  }
  
  setInverse <- function(i) inv <<- i
  getInverse <- function() {inv}
  list(set=set, get=get, setInverse=setInverse ,getInverse=getInverse)
}


## Return a matrix that is the inverse of 'mc'
cacheSolve <- function(mc, ...) {
  
  if (is.null(mc$getInverse())) {
    message("computing")
    mc$setInverse(solve(mc$get(), ...))
  }
  
  mc$getInverse()
  
}

