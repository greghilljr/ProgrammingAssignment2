## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(y) {
                m <<- x
                i <<- NULL
        }
        get <- function() return(m)
        set_inv <- function(inv) i <<- inv
        get_inv <- function() return(i)
        return(list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv))
}


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
        i <- m$get_inv()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- m$get()
        i <- solve(data, ...)
        m$set_inv(i)
        return(i)
}
