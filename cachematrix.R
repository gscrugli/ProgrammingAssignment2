## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than computing it repeatedly (there are also alternatives to matrix inversion that we will not discuss here). 
## My functions cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinvers <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        i <- x$getinversen()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(i)
        i
}
