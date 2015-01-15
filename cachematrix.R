## makeCacheMatrix and cacheSolve functions are used to cache the results when we compute an inverse of a matrix
## This avoids inverse calculation when we solve for the matrix that we already have the value for

## This function returns a list of functions that can create a matrix, print a matrix
## Calculate Inverse of a matrix and to Print the Inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {

        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Searches Cache for the inverse of matrix provided
## Calculates,prints and caches the inverse if the result is not cached earlier
## Prints the result if the inverse is already cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
