## These function will compute a matrix inversion and cache the result for later use.
## It will take a matrix as input and create functions to calculate the inverse if it has not been 
##        calculated or retrieve the result from cached memory if already stored.

## Takes a matrix as input and creates a list of functions to retrieve the matrix, inverse, or store 
##      the inverse information to cache memory upon calculation.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Takes a matrix as input and checks to see if the inverse has been computed.  If so, it retrieves
##      the information and returns.  If not, the inverse is calculated and stored in cache memory.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
