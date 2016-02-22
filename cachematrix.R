## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix: 
# function that sets and retrieves the data of a given matrix
# also set and get the cached inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    # Set the matrix to the passed argument.
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    # Get the matrix
    get <- function() x
    
    # Set the inverse matrix
    setInverse <- function(Inv) m <<- Inv
    
    # Get the inverse matrix
    getInverse <- function() m
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
# cacheSolve:
# returns the inverse of a matrix.
# first looks for cached inverse.
# if cached inverse doesnt exist then compute inverse using solve()
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    #check to see if inverse is already cached
    m <- x$getInverse()
    #if it is cached, get the cached matrix and skip computation.
    if(!is.null(m)) {
        message("getting cached data")
        # return cached data
        return(m)
    }
    
    #calculate inverse using solve if not cached
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
