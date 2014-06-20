## The following functions take the inverse of a
## a matrix and cache it.

## This function takes the argument of a matrix 
## and creates a special object that can cache its
## inverse.  It is assumed that the matrix supplied
## is always invertible.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, 
       setsolve = setsolve,
       getsolve = getsolve)
}

## This function computes the inverse of a special
## matrix returned by makeCacheMatrix. If the inverse
## has already been calculated, then the function
## will retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data) %*% data
  x$setsolve(m)
  m
}
