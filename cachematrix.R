## These functions allow one to cache the inverse of a matrix so as not to have to 
## recompute it if used in a loop.

## This function creates a list object with functions that recall or set the initial matrix
## or recall or set the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## This function checks to see if the matrix inverse exists. If so, it returns it; if not it computes
## it and then stores it.

cacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("Cached data returned - no computations performed")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
}