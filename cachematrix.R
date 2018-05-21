## The following functions will create a cachable matrix and compute the invers
## of a matrix (either using a cached solution or computing it on the fly)

## This function creates a special 'matrix' object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## Initiate variable for the inverse
    inv <- NULL
    
    ## Set function for the matrix & the cached inverse
    set <- function(y)  {
        x <<- y
        inv <<- NULL
    }
    
    ## Get function for the matrix
    get <- function() x
    
    ## Set function for the inverse
    setInverse <- function(inverse) inv <<- inverse
    
    ## Get function for the inverse
    getInverse <- function() inv
    
    ## Return functions
    list(set=set,
         get=get,
         setInverse=setInverse,
         getInverse=getInverse)
}


## This function computes the inverse of the special 'matrix' returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then the cachesolve should retrieve the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
    
    ## Get cached inverse and return if not null
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Getting cached data on the inverse of the matrix")
        return(inv)
    }
    
    ## Continue here if cached inverse is NULL to calculate and return inverse
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv
}
