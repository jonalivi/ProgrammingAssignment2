## These two functions provide a means for a matrix inverse
## calculations. The calculations occur only if the
## specified matrix inverse has not been computed yet;
## else it is returned from the cache.
##
## Usage:
## x <- makeCacheMatrix(y)
## z <- cacheSolve(x)
## y - a parameter specifying an invertible matrix

## Function makeCacheMatrix converts an ordinary
## matrix specified in the x parameter to an object
## which stores its matrix data and the currently
## calculated matrix inverse internally.

## The matrix can be accessed through get() and set(y).
## The matrix inverse can be accessed through
## getInverse() and setInverse(y).

makeCacheMatrix <- function(x = matrix()) {
    ## initialize the matrix inverse internal variable
    inv <- NULL
    ## define a setter for the matrix variable
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## define a getter for the matrix variable
    get <- function() x
    ## define a setter for the matrix inverse variable
    setInverse <- function(inverse) inv <<- inverse
    ## define a getter for the matrix inverse variable
    getInverse <- function() inv
    ## return a list of all of the functions
    list(set = set, get = get, setInverse = setInverse,
         getInverse = getInverse)
}



## Function cacheSolve computes the matrix inverse value
## (if it has not been computed before) or retrieves the
## cached result from the environment.

## The function takes the list returned by the cacheSolve()
## function as an argument

cacheSolve <- function(x, ...) {
    ## retrieve the value of the matrix inverse
    inv <- x$getInverse()
    ## if the value is defined, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    ## else get the 'raw' matrix data and compute the inverse
    ## via the standard solve() function
    data = x$get()
    inv <- solve(data)
    ## store the newly calculated value to the cash
    x$setInverse(inv)
    ## return the result
    inv
}
