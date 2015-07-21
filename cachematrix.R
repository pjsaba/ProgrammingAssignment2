## Functions below implements a cache mechanism that avoid unnecessary 
## calculation of inverse of a matrix

## makeCacheMatrix is a function that creates a special type of "matrix"
## that stores not only its content but also its inverse. 
## Note: A matrix created by makeCacheMatrix is really a list containing 
## getters and setters to itself and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    set <- function (y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function () {
        x
    }
    
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    getInverse <- function() {
        inv
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve return an inverse of a matrix passed as parameter. If the matrix 
## already has its inverse calculated, cacheSolve just retrieves its value.
## Otherwise if its inverse isn't defined, the function calculates it and 
## cache into the own matrix.

cacheSolve <- function(x, ...) {
    inv <- x$getInverse()
    
    if (!is.null(inv)) {
        message("Getting cached data...")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
