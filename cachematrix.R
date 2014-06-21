##  This function takes a matrix stores it (under variable "x") and returnes a list of functions. These functions are called by the "cacheSolve" function to handle the matrix inversion and to set the status of the matrix inversion (see "cacheSolve function" for explenation about the status mechanism).

## Stores matrix and returnes list of functions
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


##  This function calls the different "makeCacheMatrix" functions according to the status of m. When a new matrix is used m=null. Therefore the inverse matrix will be calculated and m will be set to the resut of this calculation (inverse matrix) and returned. When the same matrix is called again m=invers matrix and therefore not null. Therefore the function will simply return m (inverse matrix) together with a messege that this is a cached data.

## Returnes the inverse matrix of x

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

