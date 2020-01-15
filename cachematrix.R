# Overall description:
# Matrix inversion is usually a costly computation and there may be some
# benefit to caching the inverse of a matrix rather than computing the same 
# matrix repeatedly. Below are two functions that are used to create a special 
# object that stores a matrix and caches its inverse.


# Start of first function-------------------------------------------------------
# Name: `makeCacheMatrix`
# Description: this function creates a special "matrix", which is a list 
# containing a function to:
#       1.  set the value of the matrix
#       2.  get the value of the matrix
#       3.  set the value of the inverse matrix
#       4.  get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        iv <- NULL
        set <- function(y) {
                x <<- y
                iv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) iv <<- inverse
        getInverse <- function() iv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}
# End of first function---------------------------------------------------------


# Start of second function------------------------------------------------------
# Name: 'cacheSolve'
# Description: this function computes the inverse of the special"matrix" 
# created by `makeCacheMatrix` above. It first checks to see if the inverse has
# already been calculated (and the matrix has not changed). If so, it retrieves 
# the inverse from the cache and skips the computation. Otherwise, it computes
# the inverse of the data and sets the value of the inverse matrix in the cache 
# via the 'setInverse' function.

cacheSolve <- function(x, ...) {
        iv <- x$getInverse()
        if(!is.null(iv)) {
                message("getting cached data")
                return(iv)
        }
        data <- x$get()
        iv <- solve(data, ...)
        x$setInverse(iv)
        iv
}
# End of second function--------------------------------------------------------

# Comments are welcomed! 
