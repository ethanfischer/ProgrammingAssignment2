## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                                 # inv is the variable that will hold the inverse
    
    set <- function(y)                          # creates the matrix
    {
          x <<- y                             
          inv <<- NULL
    }
    get <- function() x                         # returns the matrix
    setinv <- function(nvrs) inv <<- nvrs       # sets x's mean
    getinv <- function() inv                    # returns x's mean
    
    list(set = set, get = get,                  # list of each function
    setinv = setinv,
    getinv = getinv)
}


## This function computes the inverse of the special "matrix"
# returned by makeCacheMatrix above. If the inverse has already been
# calculated (and the matrix has not changed), then cacheSolve should
# retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()                       # store x's inv in 'm'
    if(!is.null(inv)) {                     # if x$getinv returned a value, return it
        message("getting cached data")
        return(inv)                         # return the cached mean and exit cachemean()
    }
    data <- x$get()                         # otherwise, store the matrix in data
    inv <- solve(data, ...)                 # calculate inverse of x and store it in 'm'
    x$setinv(inv)                           # set the inverse of x for future use
    inv                                     # return the inverse
}