##Functions to calculate the inverse of a matrix
##since this calculation is a heavy one, these functions try to cache the 
##the inverse so it will be retrieved faster.


###makeCacheMatrix creates a special type of matrix, that can do the following:
##$set :: sets the matrix data inside the object
##$get :: gets the matrix data from the object
##$setInverse :: sets the inverse of the matrix to cache
##$getInverse :: gets the cached inverse of the matrix
######
##Input  ::  square matrix that can be inversed.
##output ::  a vector of functions 'object' described above.
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


##Input  :: the object returned from makeCachMatrix 
##output :: returns the inverse of the matrix, 
##          it tries to load the cached version 
##          if it fails it will calculate it and cache it for later use.
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
