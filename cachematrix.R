## The functions below are used to create a special object that stores a matrix and caches its inverse.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setsolve <- function(inverse) i <<- inverse
    getsolve <- function() i
    list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## This function computes or retrieves from cache the inverse of the special "matrix" returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getsolve()
    if (!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setsolve(i)
    i
}
