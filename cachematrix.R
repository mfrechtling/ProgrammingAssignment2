## The following functions will calculate and cached the inverse of an input
## matrix, allowing the inverse to be retreived from the cache at a later point
## rather than being recalculated

## Get and set functions for the input matrix and it's inverse

makeCacheMatrix <- function(x = matrix()) {
        xinv    <- NULL
        set     <- function(y) {
                x       <<- y
                xinv    <<- NULL
        }
        get     <- function() x
        setinv  <- function(inv) xinv <<- inv
        getinv  <- function() xinv
        list(set = set, get = get,
                setinv = setinv, 
                getinv = getinv)
}

## Checks to see if the inverse of the input matrix is in the cache and returns
## it if found. Otherwise calculates and caches the inverse before returning it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data    <- x$get()
        inv     <- solve(data)
        x$setinv(inv)
        inv
}
