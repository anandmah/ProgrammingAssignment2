## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse of matrix
# get the value of the inverse of matrix

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) minv <<- solve
        getInverse <- function() minv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
        
        
}


## Write a short comment describing this function
# The cacheSolve function calculates the inverse of the special "matrix" created with the above function. 
# It first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the 
# setInverse function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        minv <- x$getInverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setInverse(minv)
        minv
}
