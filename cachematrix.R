## Assignment: Caching the Inverse of a Matrix

## Matrix inversion is usually a costly computation and there may be some benefit
## to caching the inverse of a matrix rather than computing it repeatedly
## makeCacheMatrix & cacheSolve are two functions to cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL # Initializes the inverse
        set <- function(y) {    # Sets the matrix
                x <<- y
                m <<- NULL
        }
        get <- function() x # Gets the matrix
        setinverse <- function(inverse) m <<- inverse # Sets the inverse of the matrix
        getinverse <- function() m # Gets the inverse of the matrix
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)   # Returns the methods
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()     
        if(!is.null(m)) {       # Returns the inverse of the matrix if it was already inverted
                message("getting cached data")
                return(m)
        }
        data <- x$get() # Gets the matrix
        m <- solve(data, ...) # Computes de inverse of a square matrix
        x$setinverse(m) # Sets de inverse
        m               # Returns the matrix
}
