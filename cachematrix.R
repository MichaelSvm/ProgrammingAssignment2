## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Description: The makeCacheMatrix() function, where the original matrix is fed into, creates an object 
## which is input to the cacheSolve() function. This object stores functions and calculated inverted matrix that 
## are called by casheSolve() in order to create the resulting inverted matrix or getting the stored version of it.


makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  # method
  get <- function() x
  # method
  setsolve <- function(solve) m <<- solve
  # method
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Write a short comment describing this function

## The cacheSolve() function checks if the matrix inverse already has been calculated in x$getsolve() and stops if it 
## has. Otherwise it continues with executing the get, getsolve and setsolve functions in makeCacheMatrix() and finally 
## returns m wich is the inversed matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}
