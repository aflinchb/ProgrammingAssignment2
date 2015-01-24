## The following functions allow for a simple way to store the value
## of a matrix as well as its inverse.

##  Function to create a special matrix with a cached inverse

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize inverse to NULL 
        inv <- NULL
    
        ## Function to set the matrix value
        set <- function(y){
            x <<- y
            inv <<- NULL
        }
    
        ## Function to get the matrix value
        get <- function() x
    
        ## Function to set the cached inverse matrix value
        setInverse <- function(inverse) inv <<- inverse
    
        ## Function to get the cached inverse matrix value
        getInverse <- function() inv
    
        ## List of available functions for the special cache matrix
        list(set = set, get = get, 
             setInverse = setInverse,
             getInverse = getInverse)
}

## Function to calculate the inverse of the special matrix 'x'
## Note: The function assumes the matrix is:
##         - numeric
##         - invertible

cacheSolve <- function(x, ...) {
        ## Retrieve and return the inverse value if already cached
        m <- x$getInverse()
        if(!is.null(m)) {
            message("getting cached data")
            return(m)
        }
    
        ## Get the matrix value
        data <- x$get()
    
        ## Calculate the inverse
        m <- solve(data, ...)
    
        ## Cache and return the calculated inverse
        x$setInverse(m)
        m
}
