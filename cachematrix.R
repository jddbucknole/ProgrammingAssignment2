## These functions will compute a matrix inversion and cache the result for later use.
## It will take a matrix as input and create functions to calculate the inverse if it has not been 
##      calculated or retrieve the result from cached memory if already stored.

## Takes a matrix as input and create a list of functions to retrieve the matrix, inverse, or 
##      store the inverse information to cache memory upon calculation.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    # Cache the matrix
    set <- function(y) {    
        x <<- y
        inv <<- NULL
    } 
    
    # Returns the matrix
    get <- function() x
    
    # Cache the matrix inverse to a variable inv
    setinv <- function(inverse) inv <<- inverse
    
    # Return the matrix inverse
    getinv <- function() inv
    
    #Return a list of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Takes a matrix as input and checks to see if the inverse has been computed.  If so, it retrieves
##      the information and returns.  If not, the inverse is calculated and stored in cache memory.

cacheSolve <- function(x, ...) {
        
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    
    # Check if inverse cached and return if so.
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    # if not cached, solve for matrix inverse and return
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv(inv)
    inv
}
