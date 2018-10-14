## This function creates a special vector that really is a list of various operations on the matrix
## By using special operator '<<-' this function is able to cache an operation on the matrix 
## in a different environment rather than the current environment.
##
## This is really interesting operation that R can do. Also test by using just the '<-' operator 
## and that doesn't work, it will recompute each time the cacheSolve function is called
##
makeCacheMatrix <- function(x = matrix()) {
    m <<- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    
    # key operation to store the inverse as cache
    setinverse <- function(inverse) m <<- inverse
    
    getinverse <- function() m
    
    list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
  
}


## This is a simple function.
## It checks for cache'd value of inverse operation. If found - return the value
## Else it would compute the inverse and sote that value in cache.
##
cacheSolve <- function(x, ...) {
    # check if inverse is cached
    m <- x$getinverse()
    
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
  
    # did not find inverse - so compute it
    data <- x$get()
    m <- solve(data, ...)
    
    # make sure to store the inverse in cache for future use
    x$setinverse(m)
    
    m
}
