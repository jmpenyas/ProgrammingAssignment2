## This file contains the functions used for the week 3 programming
## assignment of Coursera's R programming
## The functions will create a cached Inverse matrix using solve function
## and will use this cached result if it exists

## Gets a matrix as input variable and provides through anonymous functions
## the following:
## 1. Get the value of the matrix
## 2. Set the value of the matrix
## 3. Get the value of the inverse matrix
## 4. Set the value of the inverse matrix
## This allows to get the last stored inversed matrix
makeCacheMatrix <- function(x = matrix()) {
      s <- NULL
      ## Set the value of the matrix
      set <- function(y) {
            ## Use of <<- because it's assigning a value to an object
            ##in an environment that is different from the current 
            ##environment
            x <<- y
            s <<- NULL
      }
      ## Set the value of the matrix
      get <- function()
            x
      ## Set the inverse of the matrix. It receives the result of the 
      ## function
      setsolve <- function(solve)
            s <<- solve
      ## Get the inverse of the matrix.
      getsolve <- function()
            s
      list(
            set = set,
            get = get,
            setsolve = setsolve,
            getsolve = getsolve
      )
      
}


## Receives a cacheMatrix object created with previous function and
## analyse whether if a cached inverted matrix already exists.
## If it does, return that matrix if not it will the compute of the
## inversion of the matrix

cacheSolve <- function(x, ...) {
      ## Get the cached inverted Matrix
      s <- x$getsolve()
      ## if it exists, return it to save computation time
      if (!is.null(s)) {
            message("getting cached data")
            return(s)
      }
      ## If not, it computes the matrix inversion
      message("executing function")
      ## Gets the matrix
      data <- x$get()
      ## Use solve for the matrix inversion
      s <- solve(data,...)
      ## Stores the result for future executions
      x$setsolve(s)
      s
}