## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        setinvertible <- function(solve) inv_x <<- solve
        getinvertible <- function() inv_x
        list(set = set, get = get,
             setinvertible = setinvertible,
             getinvertible = getinvertible)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$getinvertible()
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$setinvertible(inv_x)
        inv_x
}
