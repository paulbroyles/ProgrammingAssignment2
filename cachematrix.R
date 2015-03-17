## Together, these functions allow the user to compute the inverse of a matrix
## using the solve function while caching the results so they do not need to be
## calculated repeatedly for the same matrix (for example, within a loop).
##
## The makeCacheMatrix function takes a matrix and creates an object capable
## of containing cached values for the solve function. The cacheSolve function
## takes the object created by makeCacheMatrix and returns the results of
## solve, either calculating it or retrieving the cached value as needed.
##
## These functions are patterned after functions for computing and caching the
## mean of a vector, given in the instructions for Programming Assignment 2 of
## the Coursera R Progamming Course (Peng, Leek, Caffo), 2015/03/02 - 03/29.


## The makeCacheMatrix function accepts a matrix and returns an object for use
## with the cacheSolve function, capable of caching the result of the solve
## function. Once the object has been created, the value of the matrix can be
## read using the x$get() function, and can be changed without recreating the
## object using x$set(y). (x$setsolve and x$getsolve should not be used
## directly; use the cacheSolve function to retrieve the inverse of the matrix
## and cache it if neecssary.)

makeCacheMatrix <- function(x = matrix()) {
    solveCache <- NULL # Initialize the variable caching the inverse as null
    
    ## The set function gives new contents to the matrix object
    set <- function(y) {
        x <<- y
        solveCache <<- NULL # If the contents are reset, nullify the cache.
    }
    get <- function() x # Return the matrix
    setsolve <- function(solve) solveCache <<- solve # Cache the results
        # We only cache here; cacheSolve handles the computation. That is to
        # avoid computing the inverse before it is needed.
    getsolve <- function() solveCache # Return cached results
    # Expose the functions using a list.
    list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The cacheSolve function accepts a matrix in the form of an object created
## with the makeCacheMatrix function and returns its inverse, using the solve
## function. If the inverse has already been calculated, cacheSolve returns the
## cached inverse; if it has not been calculated, cacheSolve computes the
## inverse, caches it, and returns the result.

cacheSolve <- function(x, ...) {
    # Try retrieving the cached results.
    solve_x <- x$getsolve()
    if(!is.null(solve_x)) {
        return(solve_x) # If we found something, we're done.
    }
    # If we didn't find a cache of the results of solve, we need to compute it.
    matr <- x$get() # Read the matrix from the object so we can calculate.
    solve_x <- solve(matr, ...) # Run solve normally on the matrix.
    x$setsolve(solve_x) # Cache the value.
    solve_x # Return the value we just calculated.
}
