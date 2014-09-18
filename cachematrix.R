## These functions cache the Inverse of a Matrix

## 'makeCacheMatrix' function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) { ## sets the value of the matrix
        x <<- y
        m <<- NULL
    }
    get <- function() x ## gets the value of the matrix
    setmatrix <- function(solve) m <- solve
    getmatrix <- function() m
    list(set = set, 
         get = get,
         setmatrix = setmatrix,
         getmatrix = getmatrix)
}

## 'cacheSolve' function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getmatrix()
    if(!is.null(m)) {   ## if inverse matrix is already cached
        message("getting cached data")
        return m
    }
    matrix <- x$get()
    m <- solve(matrix, ...)   ## cache inverted matrix
    x$setmatrix(m)
    m
}
