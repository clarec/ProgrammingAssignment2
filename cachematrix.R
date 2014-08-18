## makeCacheMatrix -> creates a special `matrix` object
##                    that can cache its inverse
## cacheSolve -> computes the inverse of the special `matrix`
##               returned by makeCache. If the inverse has
##               already been calculated then the inverse
##               is retrieved from the cache


## ----------------------------------------------------------
## This function creates a special `matrix` object that can 
## cache its inverse

makeCacheMatrix <- function(A = matrix()) {
  inv <- NULL
  # set the value of the matrix
  set <- function(B) {
    A <<- B
    inv <<- NULL
  }
  # get the value of the matrix
  get <- function() A
  # set the inverse of the matrix
  setinverse <- function(solve) inv <<- solve
  # get the inverse of the matrix
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse,
       getinverse = getinverse)
}

## ----------------------------------------------------------
## This function computes the inverse of the special `matrix
## returned by makeCache. If the inverse has already been 
## calculated
## then the inverse is retrieved from the cache

cacheSolve <- function(A, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## if inverse exists get cached data
  inv <- A$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  ## otherwise calculate the inverse
  data <- A$get()
  inv <- solve(data, ...)
  A$setinverse(inv)
  inv
}
