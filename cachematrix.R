## Overview of cachematrix.R
##
## These functions provide a mechanism to create an object that contains a matrix
##    and functions to update and retreive the matrix.  The object also contains 
##    functions to create and retreive the inverse of the matrix.  Once the inverse
##    is calculated it will be saved for future retreival.
##
## Example: mt <- replicate(4,rnorm(4))    # Create a Random 4x4 matrix
##          cm <- makeCacheMatrix(mt)      # Create a Cache Matrix
## 
##          cm$get()                       # Return the original Matrix
##          cm$set(replicate(4,rnorm(4)))  # Save a new matrix to the object and Reset the Cached Inverse
##          cm$getsolve()                  # Return the Cached Inverse if it exists, otherwise Null
##          cm$setsolve()                  # Save the Cached Inverse
##
##          cacheSolve(cm)                 # If first call, calculate inverse and save
##          cacheSolve(cm)                 # Otherwise return saved inverse
##
## Function makeCacheMatrix:
##   Create an object that caches a matrix and it's inverse
##   Return a list of Functions that can be callued using the x$function syntax
##   Functions Returned:
##      set      ==> Update the Value of the Cached Matrix and reset the Cached Inverse Matrix
##      get      ==> Retreive the Cached Matrix
##      setsolve ==> Calculate the Inverse of the Cache Matrix and Save
##      getsolve ==> Retrieve the Cache Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setsolve <- function(solve) {m <<- solve}
  getsolve <- function() {m}
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}


## Function cacheSolve:
##   Using a CacheMatrix object, check if the Inverse Matrix
##     has been cached yet.  If it has return it otherwise
##    calculate the inverse and save it to the cache

cacheSolve <- function(x, ...) {
  message("checking for cached data")
  m <- x$getsolve()
  if(!is.null(m)) {
    message("cached data found")
    message("returning inverse")
    return(m)
  }
  message("cached data not found, getting data")
  data <- x$get()
  message("Solving inverse of data")
  m <- solve(data, ...)
  message("saving cached inverse")
  x$setsolve(m)
  message("returning inverse")
  return(m)
}
