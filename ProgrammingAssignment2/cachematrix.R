## The following functions allow the user to cache the inverse of a square invertible matrix
## if it is going to be used multiple times. For example in a loop. 
## Once the inverse is calculated once it can be looked up in the cache rather than recomputed
## every time after the first.

## The function makeCacheMatrix creates as special matrix object, a list, containing a function
## to do ech of the following
##1. set the value of the matrix
##2. get the value of the matrix
##3. set the value of the inverse
##4. get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
    

