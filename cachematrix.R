## This code contains two functions - makeCacheMatrix and cacheSolve
## Code: makeCacheMatrix
## Author: Venkatesh Thyagarajan
## Description: This function creates a special "matrix" object that can cache its inverse.
## 1. Initialize the variable m to NULL
## 2. Set the value of the function using argument y
##     Initializing x with y and m with NULL in global environment
## 3. Get the value of function by passing x
## 4. Set setsolve to function value using solve as an argument
## 5. Set get solve to function value using m
## 6. Build list

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x=matrix(), ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}        ## Return a matrix that is the inverse of 'x'
