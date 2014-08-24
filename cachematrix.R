## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function( x = matrix() ) {
        m <- NULL
        set <- function( matrix ) {
                x <<- matrix
                m <<- NULL
        }
        get <- function() m
        setMatrix <- function(solve) m <<- inverse
        getMatrix <- function() m
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        m <- x$getMatrix()
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setMatrix(m)
        m
}