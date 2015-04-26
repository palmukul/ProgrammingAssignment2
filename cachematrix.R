## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The first function, makeCacheMatric creates a  "matrix", which is really a list containing a function to
## Set the value of the matrix, get the value of the matrix, set the value of the inverse, get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv<- NULL
        set <- function(y) {
                x <<- y
                inv<<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
## This function returns the inverse of a matrix
## The following function calculates the inverse of the "matrix" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the solve function.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("get data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}

##> m = matrix(c(2,2,3,2), nrow=2, ncol=2, byrow = TRUE)
##> m
##     [,1] [,2]
##[1,]    2    2
##[2,]    3    2
##> n = makeCacheMatrix(m)
##> n$get()
##     [,1] [,2]
##[1,]    2    2
##[2,]    3    2
##> cacheSolve(n)
##     [,1] [,2]
##[1,] -1.0    1
##[2,]  1.5   -1


