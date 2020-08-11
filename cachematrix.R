## These set of functions cache the inverse of a matrix to help save on
## computation in case the inverse of matrix is already available


## makeCacheMatrix function provides a list of methods/functions to 
## set the value of matrix
## get the value of matrix
## set the inverse of matrix
## get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## initialize the matrix inverse to NULL
    # Set the matrix values.
    # Inverse of matrix is set to NULL -- cache cleared.
    set <- function(y){
        x <<- y
          i <<- NULL  ## set the inverse to NULL for matrix value changes
    }
    # Return the matrix
    get <- function() x
    # Set the matrix inverse
    setinv <- function(inv) i <<- inv
    # Return the matrix inverse
    getinv <- function() i
    # List of methods exposed by the function
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve function check the cache if inverse of input matrix is available
## If available it returns the same, else computes stores and returns the inv
## Further description in-line with the code.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # Get the current cached inverse
        i <- x$getinv()
        # Check validity of cache and return inverse if valid
        if(!is.null(i)){
            message("Getting cached matrix inverse")
            return(i)
        }
        # Cache Invalid - computing inverse, storing and returning the same.
        data <- x$get()
        i <- solve(data,...)  ##Using solve function for matrix inverse
        x$setinv(i)           ## Cache the computed Inverse
        i
}
