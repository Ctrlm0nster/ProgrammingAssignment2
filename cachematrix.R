## Put comments here that give an overall description of what your
## functions do: The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse. The cacheSolve function computes the inverse of
## the special "matrix" returned by makeCacheMatrix. If the inverse has
## already been calculated (and the matrix has not changed), then cacheSolve
## retrieves the inverse

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  mat_inverse <- NULL # initialize the inverse matrix as NULL
  set <- function(y) {
    x <<- y # assign new matrix to x in parent environment
    mat_inverse <<- NULL # reset inverse matrix to NULL
  }
  get <- function() x # return the matrix x
  setinverse <- function(inverse) mat_inverse <<- inverse # set the inverse matrix
  getinverse <- function() mat_inverse # return the inverse matrix
  list(
    set = set, get = get, # return a list of functions
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  mat_inverse <- x$getinverse() # get the cached inverse matrix
  if (!is.null(mat_inverse)) {
    message("getting cached data")
    return(mat_inverse)
  }
  data <- x$get() # get the matrix x
  mat_inverse <- solve(data, ...) # compute the inverse matrix
  x$setinverse(mat_inverse) # cache the inverse matrix
  mat_inverse
}
