## These functions aim at calculating the inverse of a square invertible matrix. 
# However, if the inverse of the matrix has been already calculated, this function 
# gets the inverse from the cache and skips the computation

## makeCacheMatrix creates a special "matrix", 
# which is a list containing four functions to
# set: set the value of the matrix
# get: get the value of the matrix
# setInverseMatrix: set the value of the inverse matrix
# getInverseMatrix: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {        
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setInverseMatrix <- function(solve) s <<- solve
        getInverseMatrix <- function() s
        list(set = set, get = get,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)
}



## The following function calculates the inverse of the special "matrix" 
# created with the above function. 
# it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the data and sets the value of the inverse in 
# the cache via the setInverseMatrix function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getInverseMatrix()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setInverseMatrix(s)
        s
}
