# Functions to compute matrix inverse. Limitation: Matrix must be invertible.
# Improves speed of standard solve function by caching matrix inverse once
#   calculated for use in subsequent calls.
#
# Usage:
#   source("cachematrix.R")
#   a <- makeCacheMatrix()
#   a$set(matrix(c(1,4,9,5,3,7,2,9,1),nrow=3,ncol=3))
#   ainv <- cacheSolve(a)
#   identity_matrix <- round(a$get() %*% ainv)

# Function makeCacheMatrix: Set, store, and retrieve matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Creates an object that caches the inverse of a matrix.
    # Stores matrix and initializes cached inverse to NULL. 
    #
    # Args:
    #   x: Matrix to be stored. Defaults to 1x1 matrix with data NA.
    #
    # Returns:
    #   An object consisting of a list of subfunctions (set, get, setinv, getinv).

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
        
    list(set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

# Function cacheSolve: Returns calculated or cached matrix inverse.

cacheSolve <- function(x, ...) {
    # Calculates inverse of a matrix if not previously cached.
    #
    # Args:
    #   x: Invertible matrix.
    #   ...: Optional arguments for standard solve function.
    #
    # Returns:
    #   Matrix inverse from solve function or cache.
    
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached inverse")
        return(i)
     }    
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
  i
}