# Between these two functions, we can create a matrix object that is able to cache its inverse
# The cacheSolve function will retreive the inverse of the matrix that's made by makeCacheMatrix

## This function stores the list of functions that are necessary to create, retrieve, and cache the
## inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve will return the inverse of the matrix x which was created by makeCacheMatrix.  
## If the inverse has been cached, it will retrieve it rather than re-calculate.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
