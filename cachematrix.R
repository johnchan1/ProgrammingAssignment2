## Matrix inversion is usually a costly computation and there may be benefits to
## caching the inverse of a matrix rather than computing it repeatedly.


## This function creates a special "matrix" object that can cache its inverse.
## Assume that the matrix supplied is always invertible.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## set the matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## get the matrix
        get <- function() x
        
        ## Computing the inverse of a square matrix 
        setMatrix <- function(solve) m <<- solve
        
        ## get the cache value
        getMatrix <- function() m
        
        ## return the list
        list(set = set, 
             get = get,
             setMatrix = setMatrix,
             getMatrix = getMatrix)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then cacheSolve will retrieve the inverse from the 
## cache.

cacheSolve <- function(x=matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        
        ## check for inverse value
        m <- x$getMatrix()
        
        ## if inverse value is avalable, get from cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        ## if not available, compute the inverse and store in cache
        matrix <- x$get()
        m <- solve(matrix, ...)
        x$setMatrix(m)
        
        ## return inverse value
        m
}