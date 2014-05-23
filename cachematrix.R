## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#makeCacheMatrix creates a special type of matrix and cache it and its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setInverse <- function(inv) inverse <<- inv
    getInverse <- function() inverse
    list(set = set, get = get,
    setInverse = setInverse,
    getInverse = getInverse)
}


## Write a short comment describing this function
##calculates from cache ( if possible ) the inverse of the specail matrix created
##by makeCacheMatrix and it caches the inverse ( if needed )
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if (!is.null(inv)) {
        message("getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv  <- solve(data, ...)
    x$setInverse(inv)
    inv
}
