## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix

## returns list of setters and getters

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinvserse = setinverse,
       getinverse = getinverse)
}


## returns the inverse of x

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
