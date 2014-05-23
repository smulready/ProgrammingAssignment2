## Script contains the following functions:
##  * makeCacheMatrix: creates a special matrix that can cache
##      its inverse. Contains four additional functions to 
##          1) set: set matrix value
##          2) get: retrieve matrix value
##          3) setInverse: set inverse value
##          4) getInverse: retrieve inverse value
##  * cacheSolve: computes the inverse of the special matrix
##      returned by "makeCacheMatrix". If the inverse has already
##      been computed and the matrix remains unchanged, the 
##      function retrieves the cached inverse.

## * makeCacheMatrix *
## Creates a special matrix that can cache it's own inverse.
makeCacheMatrix <- function(x = matrix()) {
    # Default inv to NULL
    inv <- NULL
    
    # Set matrix function
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    # Get matrix function
    get <- function() x
    
    # Set Inverse of Matrix function
    setinverse <- function(inverse) inv <<- inverse
    
    # Get Inverse of Matrix function
    getinverse <- function() inv
    
    # Return list of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## * cacheSolve *
## Computes the inverse of the special matrix returned by 
##  the function "makeCacheMatrix".
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    # Retrieve inverse from x
    <- x$getinverse()
    
    # If inverse exists return stored value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # Otherwise retrive matrix and calculate inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    # Save inverse for future calls
    x$setinverse(inv)
    
    # Return inverse
    inv
}