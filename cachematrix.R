## makeCacheMatrix() - Creates fuction list to get, set, getinverse, setinverse
##                     in order to cache output. 
## cacheSolve()      - Solves the inverse of a matrix but checks first to see if it's cached

## Write a short comment describing this function

# Create matrix object that can cache its inverse

makeCacheMatrix <- function(x = numeric()) {
  
  # From the makeVector example, define four functions
  # get(), set(), setinverse(), getinverse()
  # Referenced as a list.. makeCacheMatrix generates list of these 4 functions
  # accepts square matrix. E.g.: mymatrix <- matrix (rnorm(16), 4, 4)
  
  m <- NULL
  # x$set()
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # x$get()
  get <- function() x

  # x$setinverse()
  setinverse <- function(solve) m <<- solve

  # x$getinverse()
  getinverse <- function() m

  # Create the list of functions so we reference them with x$FUNCNAME in cachesolve()
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cachesolve() checks if the solved matrix exists. 
## If so, returns the solved matrix
## Otherwise, computes then stores it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Call getinverse against matrix x:   x <- matrix(rnorm(16), 4, 4), for example
  # First try to get m
  m <- x$getinverse()
  
  # If not null (i.e., returns a value because it's already set), then return m
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Otherwise, call solve 
  # Enter the matrix.. i.e., get the original square matrix
  data <- x$get()
  # Solve the matrix.. There is no spoon
  m <- solve(data, ...)
  # now cache m
  x$setinverse(m)
  m
}

