
## This function receives a matrix 'x' and returns a special 
## matrix object that can cache its inverse.
## This special matrix is a list of functions: 
##    set: set the value of the matrix
##    get: get the value of the matrix 
##    setsolve: set the value of the inverse
##    getsolve: get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) s <<- solve
        getsolve <- function() s
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## This function calculates the inverse of the 
## special matrix obtained from makeCacheMatrix.
## If the inverse has already been calculated, and
## the matrix has not changed, this function gets
## the inverse from the cache.
## The matrix must to be square and invertible.

## Example of use:
## x <- matrix(c(1,3,2,4), nrow=2, ncol=2)
## vec <- makeCacheMatrix(x)
## cacheSolve(vec)
## cacheSolve(vec) ## it returns data from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getsolve()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setsolve(s)
        s
}
