# Functions to compute matrix inverse using special "matrix" object.
#
# Improves speed of standard solve function by caching matrix inverse once
#   calculated for use in subsequent calls.
#
# Limitation:
#   Matrix must be square and invertible.
#
# Usage:
#   source("cachematrix.R")
#   a <- makeCacheMatrix()
#   a$set(matrix(c(1,4,9,5,3,7,2,9,1),nrow=3,ncol=3))
#   ainv <- cacheSolve(a)
#   identity_matrix <- round(a$get() %*% ainv)

# Function `makeCacheMatrix`:
#   Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Creates an object that caches the inverse of a matrix.
    # Stores matrix and initializes cached inverse to NULL.
    #
    # Args:
    #   x: Matrix to be stored. Defaults to 1x1 matrix with data NA.
    #
    # Returns:
    #   An object consisting of a list of subfunctions (set, get, setinv, getinv).

    # Intitialize inverse
    i <- NULL
    
    set <- function(m) {
        # Stores the matrix and sets its inverse to NULL.
        #   Args:
        #       m: Matrix to be stored.
        #   Returns:
        #       NULL.

        x <<- m
        i <<- NULL
    }
    
    get <- function() x
        # Retrieves the stored matrix.
        #   Args:
        #       None.
        #   Returns:
        #       Stored matrix.
    
    setinv <- function(inv) i <<- inv
        # Caches the inverse for the matrix.
        #   Args:
        #       inv: Inverse of the matrix to be cached.
        #   Returns:
        #       Inverse of the matrix just cached.
    
    getinv <- function() i
        # Retrieves the cached inverse.
        #   Args:
        #       None.
        #   Returns:
        #       Cached inverse of the matrix if it has been set, NULL otherwise.
    
    # Return list of subfunctions.
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

# Function `cacheSolve`:
#   Computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
#   If the inverse has already been calculated (and the matrix has not changed),
#   then `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
    # Calculates inverse of a matrix if not previously cached.
    #
    # Args:
    #   x: Matrix object.
    #   ...: Optional arguments for standard solve function.
    #
    # Returns:
    #   Matrix inverse from solve function or cache.
    
    i <- x$getinv()
    if(!is.null(i)) {
        # Inverse already cached.
        message("getting cached inverse")
        return(i)
     }
    # Calculate, cache, and return inverse.
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}