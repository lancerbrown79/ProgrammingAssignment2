## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix initiates 2 objects (x = matrix; s = solve for matrix inverse) and
## builds a list of 4 named functions (set(), get(), setsolve(), and getsolve()). 
## The set function assigns the value of x in the parent environment and assigns 
## s as NULL to remove any previously defined values for s. The get function retrieves 
## the value of x, the getsolve function retrieves the matrix inverse (if already calculated 
## in cache), and setsolve is able to set the matrix inverse if it is not previously calculated.

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

## Write a short comment describing this function

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