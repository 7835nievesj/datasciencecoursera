## Put comments here that give an overall description of what your functions do

# makeCacheMatrix
        ## Function makes special matrix that can cache its inverse
        ## "get" returns vector "x" in main function 
        ## "set" changes vector in main function
        ## "set.inv" & "get.inv" store imput value in "inv"
        ## "set.inv" store input value in makeCacheMatrix
        ## "get.inv" returns makeCacheMatrix with stored value

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set.inv <- function(solve) inv <<- solve
        get.inv <- function() inv
        list(set = set ,get = get,
             set.inv = set.inv,
             get.inv = get.inv)
}

# cacheSolve
## Function computes inverse of special matrix returned by makeCacheMatrix
        ## if inverse already calculated cacheSolve retreives inverse from cache
        ## if inverse not calculated "data" gets matrix from makeCacheMatrix
        ## "inv" calculates inverse
        ## "x$set.inv(inv) stores inverse in "inv" from makeCacheMatrix

cacheSolve <- function(x, ...) {
        inv <- x$get.inv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set.inv(inv)
        inv
}