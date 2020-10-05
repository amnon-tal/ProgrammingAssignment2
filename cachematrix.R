## Functions to calculate the inverse of a matrix in a fast way using cache.


## Returns an object suitable for cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setinv <- function(inverse) {inv <<- inverse}
  getinv <- function() inv
  list(set = set, get = get, setinv = setmean, getmean = getmean)
}


## Returns the inverse of a matrix. If it was calculated beforehand,
## uses the saved value, otherwise calculates it and stores it in cache.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if (!is.null(inv)) {
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
