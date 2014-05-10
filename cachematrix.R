## Put comments here that give an overall description of what your
## functions do

library(MASS)

## Write a short comment describing this function

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


## This is Aaron's version of the file

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if (!is.null(inv))
  {
    message("getting cached data")
    return(inv)
  }
  
  data <- x$get()
  
  #If square, we can use solve()
  if (nrow(data) == ncol(data))
    inv = solve(data, ...)
  else
    inv = ginv(data, ...)
  
  #Set the cached value
  x$setinv(inv)
  inv
}
