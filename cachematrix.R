## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(m = matrix()) {
        # create empty variable for matrix
        i <- NULL
        # Sets the value of the variable as the result of function(y)
        set <- function(y) {
                m <<- m
                i <<- NULL
        }
        # Gets the value of the matrix
        get <- function() return(m)
        # Sets the value of the matrix as the result of function(inv)
        set_inv <- function(inv) i <<- inv
        # Gets the value of the matrix and returns it
        get_inv <- function() return(i)
        return(list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv))
}


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
        # Gets the value of the matrix created by makeCacheMatrix and assigns it to variable i
        i <- m$get_inv()
        # Checks to see whether the inverse has been calculated, and if so, retrieves it
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        # If not, applies get function to matrix and assigns result to variable data
        data <- m$get()
        # Applies inversion through solve command and returns result i
        i <- solve(data, ...)
        m$set_inv(i)
        return(i)
}
