## These set of functions cache the inverse of a matrix to help save on
## computation in case the inverse of matrix is already available


## makeCacheMatrix function provides a list of methods/functions to 
## set the value of matrix
## get the value of matrix
## set the inverse of matrix
## get the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL ## initialize the matrix inverse to NULL
    set <- function(y){
        ## No need to set value and lose the inverse if value being set is same
        if(x != y){
          x <<- y
          i <<- NULL  ## set the inverse to NULL for matrix value changes
        }
    }
    get <- function() x
    setinv <- function(inv) i <<- inv
    getinv <- function() i
    list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## cacheSolve function check the cache if inverse of input matrix is available
## If available it returns the same, else computes stores and returns the inv

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        if(!is.null(i)){
            message("Getting cached matrix inverse")
            return(i)
        }
        data <- x$get()
        i <- solve(data)  ##Using solve function for matrix inv
        x$setinv(i)
        i
}
