## Get the inverse of a matrix (if is invertib)

## Insert a metrix as argument for the function

makeCacheMatrix <- function(m = matrix()) {
        i <- NULL
        set <- function(x) {
                m <<- x
                i <<- NULL
        }
        get <- function() m
        set.inverse <- function(inverse) i <<- inverse
        get.inverse <- function() i
        list(set = set,
             get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse
             )

}


## Write a short comment describing this function

cacheSolve <- function(m, ...) {
        i <- m$get.inverse()
        if(!is.null(i)) {
                message("Getting...")
                return(i)
        }
        ## Return a matrix that is the inverse of 'm'
        data <- m$get()
        i <- solve(data, ...)
        m$set.inverse(i)
        i
