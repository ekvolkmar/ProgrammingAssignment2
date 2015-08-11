## The following 2 functions create the inverse of a matrix 
## as caches it.  This is done to eliminate  repeated calculations as
## it can be costly and time consuming.  If there are no changes when
## the inverse is requested, it will pull the data from cache.


## This function creates a list of available functions to:
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize the inverse variable
    inv <- NULL
    
    ## Method used to set the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## Method used to get the matrix
    get <- function() {
        ## returns the matrix values
        x
    }
    
    ## Method used to set the inverse of the matrix
    setInverse <- function(inverse) {
        inv <<- inverse
    }
    
    ## Method used to get the inverse of the matrix
    getInverse <- function() {
        inv
    }
    
    ## Return the list of the methods
    list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## This function computes the inverse of the matrix returned by makeCacheMatrix function above. 
## If the inverse has already been calculated and there have been no changes to the matrix, 
## then cacheSolve function will retrieve the inverse matrix from the cache.
## Note: It is assumed that the matrix is always invertable (square)

cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    ## If the inverse matrix is already set, return it
    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## Get the matrix
    m <- x$get()
    
    ## Inverse the matrix
    inv <- solve(m) %*% m
    
    ## Set the Inverse to the variable
    x$setInverse(inv)
    
    ## Return the matix
    inv
}

##
## Testing Scenarios
##
## > x <- rnorm(25)
## > dim(x) <- c(5,5)
## > m = makeCacheMatrix(x)
## > m$get()
## > cacheSolve(m)
## > cacheSolve(m)
##
## Should result in statement "getting cached data" and no errors from any statement
##
## > x <- rbind(c(2,7,13,21,35),c(6,3,9,15,22),c(42,21,4,27,62),c(8,49,37,32,43),c(16,19,27,2,81))
## > m = makeCacheMatrix(x)
## > m$get()
## > cacheSolve(m)
## > cacheSolve(m)
##
## Should result in statement "getting cached data" and no errors from any statement