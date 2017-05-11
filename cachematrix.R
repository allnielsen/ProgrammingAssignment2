## Caching and inverting a matrix
## makeCacheMatrixc creates a special matrix object that is able to cache its inverse

## makeCacheMatrixc creates a special matrix object that is able to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(inverse){
    inv <<- inverse
  } 
  getInverse <- function() {
    inv }
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}



## cacheSOlve computes the inverse of the special matrix object created by makeCacheMatrix
## If the reverse matrix has already been calculated and the matrix has not changed, the
## cacheSolve function will get the inversed matrix from the cache

cacheSolve <- function(x, ...) {
  cachedInv <- x$getInverse()
  if(!is.null(cachedInv)) {
    message("getting cached data")
    return(cachedInv)
  }
  data <- x$get()
  cachedInv <- solve(data, ...)
  x$setInverse(cachedInv)
  cachedInv
}
