## Inverse of a Matrix Caching:
## Matrix inversion a costly computation and 
## some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Below are a few functions that are used to create a special object that stores a matrix and caches its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
            getInverse = getInverse)
}



 ## Matric that returns the inverse of 'x'
cacheSolve <- function(x, ...) {
       inv <- x$getInv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInv(inv)
        inv
}
