#cache inverse of A where A is a square matrix.
#assume that the matrix supplied is always invertible
#   > source('cacheMatrix.R')
#   > c = rbind(c(1, -1/4), c(-1/4, 1))
#   > x<-makeCacheMatrix(c)
#   > cacheSolve(x)
#   [,1]      [,2]
#   [1,] 1.0666667 0.2666667
#   [2,] 0.2666667 1.0666667
#   > cacheSolve(x)
#   getting cached data
#   [,1]      [,2]
#   [1,] 1.0666667 0.2666667
#   [2,] 0.2666667 1.0666667

#This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse  <- function(Inverse) m <<- Inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
#should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}