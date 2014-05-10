## Theses functions will "wrap" a matrix and allow us to cache the matrix's
## inverse.  The first function returns a list of functions and second 
## function computes the inverse or returns a cached value if possible

## The function is the "class" for the matrix and returns a way to access
## function to get and set the matrix and get and set its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<-NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list( set = set, get = get, setinv = setinv,
        getinv = getinv)
}


## This function returns the inverse of the matrix, as determined by the
## "solve" function.  I assume that any matrix I am dealing with can
## be inverted with solve.  This function passes along any additional
## parameters to solve (assuming the inverse is not returned from the cache)

cacheSolve <- function(x, ...) {

  inv <- x$getinv()
  if (!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  #Call solve and pass along additional parameters
  inv = solve(x$get(), ...)
  
  #Set the cached value
  x$setinv(inv)
  
  #return the inverse
  inv
}
