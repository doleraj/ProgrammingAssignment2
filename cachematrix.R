## Methods which enable generating the inverse of a square matrix, and caching
## that inverse after calculation for later lookups.

## Make a specialized vector/class with getters and setters for both the matrix
## and the inverse.

makeCacheMatrix <- function(x = matrix()) {
        storedInverse <- NULL
        set <- function(newMatrix) {
                x <<- newMatrix
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) {
                storedInverse <<- inverse
        }
        getinverse <- function() storedInverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Performs the actual inverse of a matrix, using the cached value if available.

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("Getting cached data.")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
