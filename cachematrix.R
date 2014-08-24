## This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function( x = matrix() ) {
        ## Initializes the inverse.
        m <- NULL
        ## Sets the matrix.
        set <- function( matrix ) {
                x <<- matrix
                m <<- NULL
        }
        get <- function() m
        ## Sets the inverse of the matrix.
        setMatrix <- function(solve) m <<- inverse
        getMatrix <- function() m
        ## Return a list.
        list(set = set, get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## This function computes the inverse of the special "matrix" returned
## by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve
## the inverse from the cache.

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'.
        m <- x$getMatrix()
        ## Return the inverse if already set.
        if( !is.null(m) ) {
                message("getting cached data")
                return(m)
        }
        ## Get the matrix.
        data <- x$get()
        ## Calculate the inverse.
        m <- solve(data)
        ## Set the inverse to the object.
        x$setMatrix(m)
        m
}