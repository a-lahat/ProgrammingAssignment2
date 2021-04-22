## My functions store a matrix inverse in cached memory in order to 
## save run time by calculating the inverse one time only.


## This function creates a list holding function to get/set the value of the
## matrix and its inverse matrix
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}


## This function returns the given matrix inverse.
## If the inverse was already calculated it is returned, 
## otherwise the function calculates the inverse, sets it and then returns.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}

my_mat <- matrix(c(4, 2, 7, 6), nrow = 2, ncol = 2)
my_mat
cached_mat <- makeCacheMatrix(my_mat)
cacheSolve(cached_mat)
