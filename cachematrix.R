# I have used solve to calculate the inversion of a matrix
# Since Inversion of a matrix is very time and memory consuming operation,Caching the
# makes more sense, Which is done by functions below

# The below function is used to
# set , get the values of matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinv <- function(solve) inverse <<- solve
        getinv <- function() inverse
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


# this function check if cache is available or not. If it is
# then it displays the result from cache via getinv() function  and skips the
# computation part else it calculates the value of inverse
# and stores it in cache via setinv() function .

cacheSolve <- function(x, ...) {
        inverse <- x$getinv()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- inv(data, ...)
        x$setinv(inverse)
        inverse
}
