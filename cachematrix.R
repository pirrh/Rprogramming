## Programming Assignment 2
##
## Functions: makeCacheMatrix
##            cacheSolve
##            makeCacheObject
##            cachedApply
##            RunTests
##
## Summary: This script contains the functions 'makeCacheMatrix' and
##          'cacheSolve' which create a cached matrix and compute
##          the inverse of the matrix respectively.
##          The implementation is based on the example in the assignment.
##          The functions 'makeCacheObject' and 'cachedApply' show how
##          the problem of caching computations could be generalized (see the
##          functions for detail). Note that the implementation of these
##          functions are just drafts to show how one could implement this.
##          In praxis the use of modules which handle caching is the way to
##          go.
##          'RunTests' runs some tests to ensure the correctness of the
##          solution.
##          The script follows Google's R Style Guide with minor modifications.

makeCacheMatrix <- function (x = matrix()) {
    # Wrap a matrix with functions that allow caching results.
    #
    # Args:
    #   x: A matrix. Note that for this assignment we want to invert the matrix,
    #      so it should be invertible. Default is the empty matrix.
    #
    # Returns:
    #   A list containing the following functions:
    #       set: Takes a matrix as argument and sets it as data for the cached
    #            matrix. This also resets the value of the cached inverse matrix
    #            to 'NULL'.
    #       get: Gets the unwrapped matrix of the cached matrix.
    #       setInv: Sets the value of the cached inverse matrix.
    #       getInv: Gets the value of the cached inverse matrix which is NULL
    #               if the inverse has not been computed before.
    # Note that these functions are part of the internal implementation and
    # should not called by the user.
    xInv <- NULL        # Initially, there is no cached value
    set <- function (y) {
        x <<- y
        xInv <<- NULL   # Need to compute the new inverse
    }
    get <- function () x
    setInv <- function(inverse) xInv <<- inverse
    getInv <- function () xInv
    list(set = set,
         get = get,
         setInv = setInv,
         getInv = getInv)
}

cacheSolve <- function (x) {
    # Return the inverse of a 'cacheMatrix'. If the inverse for this matrix
    # has been computed before, the cached value is returned and the computation
    # is not executed again.
    #
    # Args:
    #   x: A 'cacheMatrix' as returned by makeCacheMatrix.
    #
    # Returns:
    #   The inverse of the matrix contained in x.
    xInv <- x$getInv()
    if (!is.null(xInv)) { # If value was computed before return the cached value
        return(xInv)
    }
    data <- x$get()
    xInv <- solve(data)   # Else compute the value, save and return it
    x$setInv(xInv)
    xInv
}

makeCacheObject <- function (x) {
    # Creates a wrapper around an object, which enables
    # caching of computed values for any function. The
    # implementation is similar to the one of 'makeCacheMatrix'
    # except that it uses a list to store the cached values.
    #
    # Args:
    #   x: Any object.
    #
    # Returns:
    #       set: Takes a matrix as argument and sets it as data for the cached
    #            matrix. This also resets the value of the cached inverse matrix
    #            to 'NULL'.
    #       get: Gets the unwrapped matrix of the cached matrix.
    #       setValue: Set the value of a cached computation.
    #       getValue: Get the value of a cached computation.
    #       getValues: Get the list with all cached values.
    values <- list()
    set <- function (y) {
        x <<- y
        values <<- list() # Reset the cached values
    }
    get <- function () x
    setValue <- function (fName, value) {
        # Add a new value to the cache
        newPair <- list() # TODO: Avoid weird, slow list concatenation
        newPair[[fName]] <- (value)
        values <<- c(values, newPair)
    }
    getValue <- function (fName) values[[fName]]
    getValues <- function () values

    list(set = set,
         get = get,
         setValue = setValue,
         getValue = getValue,
         getValues = getValues)
}

cachedApply <- function (f, x) {
    # Apply a function to a cache-object. If the function has been computed
    # befor the cached value is returned. Else the function is computed and
    # the value is cached and returned.
    #
    # Args:
    #   f: A function that works on the data in 'x'.
    #   x: A cache object as returned by makeCacheObject,
    #      that contains some data.
    #
    # Returns:
    #   The result of the computation of 'f' on the data stored in 'x'.
    #
    # TODO:
    #   Arguments to the computed functions are not stored so that
    #   the cached values are just stored for the functions itself, not its
    #   parameters.
    #   This implementation uses a list to store the cached values which is
    #   definitely not the most efficient way. Using a hash or some tree-
    #   structure is better.
    funName <- as.character(substitute(f)) # Name of function as string
    value <- x$getValue(funName)
    # If there is a cached value return it ...
    if (!is.null(value)) return(value)
    # ... else compute, cache and return it
    data <- x$get()
    value <- f(data)
    x$setValue(funName, value)
    value
}

RunTests <- function (nTests = 100) {
    # Runs a small test suit to check functionality of the implemented
    # functions.
    #
    # Arg:
    #   nTests: Defines the number of tests to run. Defaults to 100.
    #
    # Returns:
    #   TRUE if every test passed. If an error occurs computation is stopped.

    # Tests the simple implementation
    for (i in 1:nTests) {
        # Define a random cache-matrix.
        # Note that randomly created matrices are invertible with very high
        # probability. Nonetheless one should always verify non-deterministic
        # computations, even if I won't here.
        d <- round(runif(1, min = 2, max = 42))
        randMat <- makeCacheMatrix(matrix(runif(d^2), d))

        # Compute the inverse and check if caching works correctly
        invMat <- solve(randMat$get())
        stopifnot(invMat == cacheSolve(randMat))
        stopifnot(invMat == randMat$getInv())
        stopifnot(invMat == cacheSolve(randMat))
        randMat$set(randMat$get()) # Reset cached values
        stopifnot(NULL == randMat$getInv())
        stopifnot(invMat == cacheSolve(randMat))
    }

    # Test the generic implementation
    # It's also possible to redefine 'makeCacheMatrix' and 'cacheSolve'
    # using 'makeCacheObject' and 'cachedApply' and run the above tests
    # again.
    for (i in 1:nTests) {
        d <- round(runif(1, min = 2, max = 42))
        randMat <- makeCacheObject(matrix(runif(d^2), d))
        invMat <- solve(randMat$get())
        stopifnot(invMat == cachedApply(solve, randMat))
        stopifnot(invMat == randMat$getValue('solve'))
        stopifnot(invMat == cachedApply(solve, randMat))
        randMat$set(randMat$get()) # Reset cached values
        stopifnot(NULL == randMat$getValue('solve'))
        stopifnot(invMat == cachedApply(solve, randMat))
    }

    TRUE
}
