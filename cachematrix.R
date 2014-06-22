## these functions are some benefit to caching the inverse of a matrix when 
## the inverse has already been calculated.

## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        ## set value of the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        ## get the value of the matrix
        get <- function() x
        ## set the value of inversed matrix
        setinverse <- function(inverse) m <<- inverse
        ## get the value of inversed matrix
        getinverse <- function() m
        list(set = set, get = get,
                setinverse = setinverse,
                getinverse = getinverse)
        }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## get inverse of 'x'
        m <- x$getinverse()
        ## If inverse has been calculated, then return message and cached value
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## if inverse has not been calculated, then calculate and return value
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
