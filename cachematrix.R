## Put comments here that give an overall description of what your
## functions do

## This function returns a list of convenience functions that support
## matrix operations in support of calculating an inverse of a matrix
## This function is called by the cacheSolve function to cache the inverse
makeCacheMatrix <- function(x = matrix()) {
    ## intialize the inverse
    Inverse <- NULL
    ## this function sets the values of a matrix which we will use as the basis
    ## for calculating the inverse matrix and caching the value.
    set <- function(y) {
        x <<- y
        ## Since we are setting values of a new matrix, we need to null out any
        ## existing inverse values in the cache.
        Inverse <<- NULL
    }
    ## return the values of the matrix
    get <- function() x
    ## cache the inverse of the matrix
    setInverse <- function(inverse) Inverse <<- inverse
    ## return the cache value of the inverse, if one exists
    getInverse <- function() Inverse
    ## function returns a list containing functions, matrix data and matrix inverse
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix function
## If the inverse has already been calculated (and the matrix has not changed), then the
## function should retreive the inverse from the cache.
## The variable argument '...' is used to pass additional variables to the solve function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Check to see if the inverse has already been set
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        ## The cache is not null, so we will return the inverse from cache
        message("getting inverse matrix from cache")
        return(inverse)
    }
    ## The cache was null, so we need to calculate it
    matrix <- x$get()
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    inverse
}
