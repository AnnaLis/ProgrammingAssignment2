# My two functions work similar to functions which were shown as an examples to this assignment

# 'makeCacheMatrix' ist function that returns a list of functions
# It store a martix and a cached value of the inverse of the matrix. 
#  This function contains the following functions:
# 1. set        set the value of a matrix
# 2. get        get the value of a matrix
# 3. setinv     set the cached value (inverse of the matrix)
# 4. getinv     get the cached value (inverse of the matrix)

makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL
  
  # store a matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # returns the stored matrix
  get <- function() x
  
  # cache the given argument 
  setinv <- function(solve) inv <<- solve
  
  # get the cached value
  getinv <- function() inv
  
  # return a list, every element of the list is a function
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

# This function 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve 
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
 # get the cached value
  inv <- x$getinv()
  # if a cached value exists, return it
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  # if the casched value doesn't exist get the matrix, caclulate the inverse and store it
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  # return the inverse of the matrix
  inv 
}
