## These functions allow a user to cache an inverse of a matrix. 
## makeCacheMatrix will cache the inverse, saving the user time, and
## cacheSolve will either return the cached data or return the inverse 
## of the new inputs

## This function allows a user to set and cache the inverse of 
## a matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y 
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve returns the inverse of a matrix. A message appears
## if the inputs matched the cached matrix, otherwise, the function
## will return the inverse of the input.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}

