## makeCacheMatrix defines and saves a list of functions in the cache
## functions do calculate the inverse of a matrux

## The function makeCacheMatrix saves the matrix x and its inverse to a list in a cache, and the functions used

makeCacheMatrix <- function(x = matrix()) {
m <- NULL ## initialization of m
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


## This function looks up whether the inverse of x has already been calculated. If yes, it is taken from
## the cache, if not, it is calculated. 

cacheSolve <- function(x = matrix(), ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()  ## m is set to the result of applying function getinverse to input matrix x - in the cache
        if(!is.null(m)) {  ## This condition is true if m is not null, i.e. a result was cached.
                message("getting cached data") ## message to user 
                return(m)   ## the cached value is returned and the function ended here
        }
        data <- x$get()   ## else: the variable data is set to the "get" function corresponding to x
        m <- solve(data, ...)  ## now, the inverse matrix of x is calculated
        x$setinverse(m)          
        m
}
