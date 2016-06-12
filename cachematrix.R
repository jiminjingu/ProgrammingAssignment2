## the purpose of the these functions is to store and retrieve a cached version of a 
## matrix inverse.

## makeCacheMatrix function is a list of four functions designed to store a matrix,
## retrieve a matrix, set the inverse of a matrix and get the invesrse from cache

makeCacheMatrix <- function(x = numeric()) {
  # creates an invese matrix and caches it if
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(mean) m <<- mean
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## cacheSolve checks fora cached inverse of a matrix, if not found it creates one

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
