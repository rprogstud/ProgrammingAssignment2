


## Given an invertible matrix, these two functions compute the inverse of a matrix
## and stores it in cache.  


## This function returns a list of functions: set,get,setinv,getinv and stores the 
## input matrix in x.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
                
        }
        get <- function() x
        setinv <- function(inv) inv <<- solve(x)
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function returns the inverse of the matrix.  It first checks to see if is 
## stored in cache and verifies the matrix has not changed from the matrix whose inverse 
## was prevously cached. If yes, it returns the cached inverse. Otherwise, it computes it,
## stores it in cache, and returns the computed inverse.


cacheSolve <- function(x, ...) {
              
        inv <- x$getinv()
        print(x)
        print(x$get)
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
