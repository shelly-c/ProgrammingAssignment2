## In the event that a matrix is incredibly large, computing the inverse each time it is
## needed can be computationally expensive. These functions together compute the inverse 
## of a matrix and cache the resulting solution matrix.


## First set the x matrix to an invertible matrix - e.g. x <- matrix(c(1,-1,1,2),nrow=2,ncol=2)
## Then call this function and store the resulting list to a variable - e.g. z <- makeCacheMatrix(x) 
## To test, this matrix can then be returned with z$get().
## If the inverse has already been calculated, z$getinv() will return the inverse.
## If the inverse has not already been calculated, z$getinv() will return NULL.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
     set <- function(y) {
     			x <<- y
             	m <<- NULL
        }
     get <- function() x
     setinv <- function(inverse) m <<- inverse
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## If the inverse of the matrix has not already been calculated, this function
## will return the message "getting cached data" then compute the inverse of the matrix.
## If it has already been calculated, this function will return the cached value of the inverse.
## From the example above, cacheSolve(z) will return the inverse of the matrix set above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x' variable from makeCacheMatrix
        ## In this function parameter, we can reuse x because it is bound to the scope of this function
        ## The 'x' in the parameters of this function do not refer to the 'x' matrix in makeCacheMatrix
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
