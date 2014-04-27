## This file contains one function to create a special matrix
## that can contain its cached inverse,
## as well as one function to calculate the inverse.
## If the inverse has been calculated once, the cached result is returned

## Create a special matrix from data
## Usage example:
## > mat <- c(1,1,1,1,2,3,3,3,1)
## > dim(mat) <- c(3,3)
## > cmat <- makeCacheMatrix(mat)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Calculate and cache the inverse of a matrix
## The matrix data are stored in a special matrix created with makeCacheMatrix
## Usage example:
## > cmat <- makeCacheMatrix(mat)
## > cacheSolve(cmat)   # Returns the calculated inverse of mat and caches it
## > cacheSolve(cmat)   # Returns the cached inverse of mat
## getting cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matrixdata <- x$get()
        inv <- solve(matrixdata, ...)
        x$setinverse(inv)
        inv
}
