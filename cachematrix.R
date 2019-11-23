## Put comments here that give an overall description of what your
## functions do
# "makeCacheMatrix" & "cacheSolve" functions create a special matrix
# object and finds its inverse using cached data in case it's available



## Write a short comment describing this function
# This function creates special "matrix" object

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y)
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# This function checks and returns cached data for matrix

cacheSolve <- function(x, ...) {
  # Returns inverse of matrix
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("Getting Cached Data!!!")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
