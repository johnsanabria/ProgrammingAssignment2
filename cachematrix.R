## Put comments here that give an overall description of what your
## functions do

## This function creates a list containing functions to
## - set the initial value of a matrix
## - get the value of a matrix
## - set the inverse value of a matrix
## - get the inverse value of a matrix

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 set <- function(y) {
  x <<- y
  m <<- NULL
 }
 get <- function() x
 setinverse <- function(inverse) m <<- inverse
 getinverse <- function() m
 list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a matrix. 
## It first checks if the inverse has already been calculated. If so, the 
## cached value is returned, otherwise the value is calculated, cached and 
## returned

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 m <- x$getinverse()
 if (!is.null(m)) {
  message("getting cached data")
  return(m)
 }
 data <- x$get()
 ## The solve method receives three arguments
 ## A square matrix 'A', a 'B' identity matrix and ellipsis. 
 ## When 'B' is missing an identity matrix is innerly calculated. Because 
 ## the ellipsis argument must be passed to the 'solve' method then this 
 ## function  explicitly invokes solve with a matrix (data), an identity matrix
 ## (diag(nrow(data))) and ellipsis.
 m <- solve(data,diag(nrow(data)), ...)
 x$setinverse(m)
 m
}
