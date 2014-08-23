## These are two functions that are used to create a special object that stores a matrix
## and caches the inverse of this matrix.

## The first funtion creates a special "matrix", which is a list of functions, 
## similar with the example presented in the description of the programing assigment. 


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function, calculates the inverse of the special "matrix" 
## created with the makeCacheMatrix.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
