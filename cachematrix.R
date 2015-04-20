## Here we device two functions:
##  makeCacheMatrix: receives a matrix and extends with the routines to store and retrieve
##                       the original data and inverse matrix
##  cacheSolve: receives the extended matrix object and either returns the stored inverse
##                 matrix or computes, stores and then returns the inverse matrix


## For a matrix x, initialize the inverse matrix pointer, store the original data.
##   Defines functions for setting and returing the original and inverse data
makeCacheMatrix <- function(x = matrix()) {
  # Initialize the stored inverseMatrix as null
  inverseMatrix <- NULL
  # When setting the stored data, drop the stored inverseMatrix
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  
  # Get simply returns the stored matrix data
  get <- function() 
          {  x }
  # setinverse stores the computed inverse of the matrix data
  setinverse <- function(inverse) 
                   { inverseMatrix <<- inverse }
  # getinverse returns the stored inverse matrix data or the null object
  getinverse <- function() 
                   { inverseMatrix }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## For a matrix x
##   Get the stored value of the inverse object.  If not null then the inverse has
##   previously been computed and should just be retuned.  Otherwise compute the inverse,
##   store it for future retreiveal, and return the inverse.

cacheSolve <- function(x, ...) {
  ## Retrieve the reference of the stored inverse matrix
  inverseMatrix <- x$getinverse()
  ## If we returned a non-null value then the inverse has been computed and can be returned
  if(!is.null(inverseMatrix)) {
    message("using cached data")
    
  } else {
    ## the inverseMatrix is null so we need to retrieve the normal matrix and compute the inverse
    data <- x$get()
    # inverse of the original matrix computed using the solve() function
    inverseMatrix <- solve(data, ...)
    # store the inverse for future cache retrieveal
    x$setinverse(inverseMatrix)
    
  }
  # Return the inverse
  inverseMatrix
}
