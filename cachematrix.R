## makeCacheMatrix and cacheSolve work together to provide a matrix inverse calclation that is stored in memory 
## so it is quicker to recall that calculation if it is needed, rather then forcing the program to recalculate it
## each time that it is needed.

## makeCacheMatrix initiates 2 objects (x = matrix; s = solve for matrix inverse) and builds a list of 4 named 
## functions (set(), get(), setsolve(), and getsolve()). the set function assigns the value of x in the parent 
## environment and assigns s as NULL to remove any previously defined values for s. The get function retrieves 
## the value of x, setsolve assigns the input argument to the value of s in the parent environment, and the 
## getsolve function retrieves the matrix inverse (if already calculated in cache).

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

## cacheSolve determines if a matrix inverse is already calculated (and is in cache) or if it needs to be
## recalculated.

cacheSolve <- function(x, ...) {
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