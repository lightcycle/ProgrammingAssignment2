## Functions for producing and using inverse-caching matrices

## Returns a list that encapsulates the provided matrix (or default empty
## matrix), and contains four functions for accessing the matrix and its cached
## inverse.

makeCacheMatrix <- function(mat = matrix()) {
    inv <- NULL
    set <- function(y) {
        mat <<- y
        inv <<- NULL
    }
    get <- function() mat
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## Returns the inverse of the provided caching matrix, calculating the inverse
## only if a cached inverse is not available

cacheSolve <- function(cachingmat, ...) {
    inv <- cachingmat$getinverse()
    if (!is.null(inv)) return(inv)
    mat <- cachingmat$get()
    inv <- solve(mat, ...)
    cachingmat$setinverse(inv)
    inv
}
