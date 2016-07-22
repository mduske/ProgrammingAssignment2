## Mentor Graded Assignment: Programming Assignment 2: Lexical Scoping 
## Course: R Programming
## Efficent caching of inverse of matrices, reuses pre-computed values whenever possible

## Create an enhancd matrix object, which is capable of keeping it inverse value
## if already computed
makeCacheMatrix <- function(x = matrix()) {
  ##CAched inverse of the matrix
  cachedInverse <- NULL
  
  set <- function(value) {
    x <- value
    cachedInverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  
  getInverse <- function() cachedInverse
  
  ##Return enhanced Matrix object
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Computes the inverse of a matrix. Whenever a previous solution is already available
## (cahced) for a given Matrix, the cached value is returned
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  cachedValue <- x$getInverse()
  
  if (is.null(cachedValue)) {
    ##Compute inverse of matrix
    inverse <- solve(x$get())
    
    ##Store result in cache
    x$setInverse(inverse)
    
    ##Return cached result
    inverse
  } else {
    ## Return cached value
    message("Cached value found!")
    cachedValue
  }
}
