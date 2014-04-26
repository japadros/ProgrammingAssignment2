## Function for Program Assignment 2, R Programming Course in Coursera
## Matrix inversion, keeping previously calculated results as a cache.

## makeCacheMatrix creates the environment and the functions for
## initialization of values.

makeCacheMatrix <- function(x = matrix()) {

  # Setting initial value to NULL to indicate nothing calculated yet
  m <- NULL
  # Setting initial value of the x member variable from the vector
  # given as argument to the set member function.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # Retreival of the x member value
  get <- function() x
  # Setting the set and get functions for usage of the object
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  # Output of list object with 4 elements
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Function for calculation and for cache retreival

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

  # Retreival of the cached value from the function's environment
  m <- x$getinv()
  # If it is not null, return the value and exit function
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  # Otherwise, receive the value from the other environment and calculate
  # inverse using solve
  data <- x$get()
  m <- solve(data, ...)
  # Set the calculated value in the other environment
  x$setinv(m)
  # Return the result
  m

}
