## These functions cache the inverse of a matrix

## This function creates a unique matrix object which can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize the inverse property
        inv <- NULL
        
        ## The below method sets the matrix
        set <- function(matrix) {
          x <<- matrix
          inv <<- NULL
        }
        
        ## The below method gets the matrix
        get <- function() {
          ## Return the matrix
          x
        }
        
        ## The below method sets the inverse of the matrix
        setInverse <- function(inverse) {
          inv <<- inverse
        }
        
        ## The below method gets the inverse of the matrix
        getInverse <- function() {
          ## Return the inverse
          inv
        }
        
        ## Return a list of methods used throughout
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the unique matrix returned by the above function 'makeCacheMatrix'.
## If the matrix remains unchanged and the inverse has already been caluclated, then 'cacheSolve'
## should return the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        
        ## Return the inverse if it has already been set
        if(!is.null(inv)) {
          message("retrieving cached data")
          return(inv)
        }
        
        ## Retrieve the matrix from the created object
        mat <- x$get()
        
        ## Calculate the inverse using matrix multiplication
        inv <- solve(mat, ...)
        
        ## Set the inverse to the object
        x$setInverse(inv)
        
        ## Return the matrix
        inv
}
