## This function creates a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  inve <- NULL
  set <- function(y) {
    x <<- y
    inve <<- NULL
  }
  get <- function() x
  setInverse <- function() inve <<- solve(x) ##computing an inverse
  getInverse <- function() inve
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## makeCacheMatrix above. If the inverse has already been calculated, and the
## matrix has not change, then the cachesolve 
##should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  
  inve <- x$getinve()
  if (!is.null(inve)) {
    message("getting cached data")
    return(inve)
  }
  data <- x$get()
  inve <- solve(data, ...)
  x$setinve(inve)
  inve
}

