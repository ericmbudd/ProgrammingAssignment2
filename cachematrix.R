## Input and store an array or matrix

## 'set' allows for manually setting a matrix
## 'get' returns a matrix stored in the object
## 'setinv' allows to manually set the inverse matrix
## 'getinv' returns the stored inverse matrix

makeCacheMatrix <- function(x = numeric()) {
    #set inverse to null since it has not been calculated
    inv <- NULL
    
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() {
        #define matrix based on input parameters
        mat <<- matrix(x,nrow=sqrt(length(x)),ncol=sqrt(length(x)))
        
        #return matrix
        mat
    }
    setinv <- function(solve) inv <<- solve
    getinv <- function() inv
    
    #define public functions
    list(set = set, get = get,
    setinv = setinv,
    getinv = getinv
    )
}

## Return a matrix that is the inverse of 'x'


cacheSolve <- function(x, ...) {
    
    inv <- x$getinv()
    
    #check if matrix calculation has been performed; if so, return cached data
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    #read input matrix
    data <- x$get()
    
    #calculate inverse matrix
    inv <- solve(data, ...)
    
    #set/return calculated inverse matrix
    x$setinv(inv)
    inv
}