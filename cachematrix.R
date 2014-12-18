## The pair of functions below allows to cache a once computed inverse matrix and use
## the cached object if we need it again

## The makeCacheMatrix function creates the special "matrix" object that can cache
## its inverse. This "matrix" object is really a list containing a function 
## to set and get a matrix and to set and get a inverse matrix cache. The
## function doesn't calculate the inverse matrix, just cache it.

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrixCache <- NULL          ## A variable for the cache
    ## The "set" function sets a new matrix
    set <- function(y) {
        x <<- y                         ## The matrix is really keeped in the
                                        ## 'x' variable, placed in the 
                                        ## 'makeCacheMatrix' function 
                                        ## environment (its formal argument)
        inverseMatrixCache <<- NULL     ## We need clear cache if the matrix
                                        ## was changed
    }
    ## The "get" function gets a matrix
    get <- function() x
    ## The "setCachedInverse" function sets the cache value
    setCachedInverse <- function(y) inverseMatrixCache <<- y
    ## The "getCachedInverse" function gets a inverse matrix from the cache
    getCachedInverse <- function() inverseMatrixCache
    ## Create and return a list of the functions above
    list(set = set, get = get, 
        setCachedInverse = setCachedInverse,
        getCachedInverse = getCachedInverse
    )
}

## The cacheSolve function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix function. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    i <- x$getCachedInverse()           ## Get a value of the cache
    if(!is.null(i)) {                   ## If the cached value exists ...
        message("getting cached data")
        return(i)                       ## ... return it and exit 
    }
    i <- solve(x$get(), ...)            ## Otherwise compute the inverse matrix
    x$setCachedInverse(i)               ## Save it in the cache
    i                                   ## and return
}
