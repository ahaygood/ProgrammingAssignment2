## makeCacheMatrix: This function creates a special "matrix" object that can 
##   cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        im <- NULL
        set <- function(y) {
                x <<- y
                im <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) im <<- inv
        getinverse <- function() im
        invisible(list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse))
        
}

## cacheSolve: This function computes the inverse of the special "matrix" 
##   returned by makeCacheMatrix above. If the inverse has already been 
##   calculated (and the matrix has not changed), then cacheSolve should 
##   retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        im <- x$getinverse()
        if(!is.null(im)) {
                message("getting cached inverse")
                return(im)
        }
        data <- x$get()
        im <- solve(data, ...)
        x$setinverse(im)
        im
}
