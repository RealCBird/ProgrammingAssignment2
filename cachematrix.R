## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly. 
## Here are a pair of functions that cache the inverse of a matrix.


## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        #storing matrix and caching variable for its inverse
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) m <<- Inv
        getInv <- function() m
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)      
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInv()
        #check if inverse matrix is already stored
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #if inverse matrix not stored, compute inverse of the matrix
        data <- x$get()
        m <- solve(data, ...)
        x$setInv(m)
        ## Return a matrix that is the inverse of 'x'
        m
}