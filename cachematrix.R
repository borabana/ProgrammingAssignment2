## Implement special matricies that cache their inverse values to optimize
## calculations involving multiple uses of the inverse.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  # Functions to track the number of cache hits and misses.  Note that these
  # values are cumulative across all values of the matrix therefore they are
  # not reset to zero when the matrix changes.
  hits <- misses <- 0
  cacheHit <- function() hits <<- hits + 1
  cacheMiss <- function() misses <<- misses + 1
  cacheEfficiency <- function() hits / (hits + misses)

  # The cached inverse.
  xInv <- NULL
  
  # Get the current value of the matrix.  Note that the formal parameter x
  # was captured by the function closures and that is why it's still available.
  get <- function() x  

  # Set a new matrix and also flush out the old cached inverse.  Note the use
  # of the <<- assignment to force updates in the environment in which the
  # matrix and its inverse were initially defined.
  set <- function(newX) {
    x <<- newX
    xInv <<- NULL
  }
  
  # Get and set the inverse for the matrix.  Again, note the use of <<- to force
  # the inverse update into the original environment.
  getInverse <- function() xInv
  setInverse <- function(newInv) xInv <<- newInv
  
  # Return a list of the functions that operate on the special matix.  This is
  # very close to returning an object in which the 
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse,
       cacheHit=cacheHit, cacheMiss=cacheMiss, cacheEfficiency=cacheEfficiency)
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been calculated (and
## the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
  # See if we have a cached inverse available.
  xInv <- x$getInverse()
  
  # If not, then compute one and cache it in the special matrix.
  if (is.null(xInv)) {
    xInv <- solve(x$get())
    x$setInverse(xInv)
    x$cacheMiss()
  }
  else {
    x$cacheHit()
  }
  
  xInv
}