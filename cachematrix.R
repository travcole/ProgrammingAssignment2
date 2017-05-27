## Matrix inversion is usually costly in time and computer resources.  Because of this
## caching may be used to reduce time when this needs to be done over several iterations

## The first function creates a special matrix that can cache its inverse. It has four
## steps. 
##        1.  Set the value of the matrix
##        2.  Get the value of the matrix
##        3.  Set the value of the matrix inverse
##        4.  Get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        mat_inv <- NULL
        set <- function (y) {
                x <<- y
                mat_inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) mat_inv <<- inverse
        getinverse <- function() mat_inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This value returns the inverse of the matrix.  It assumes that the matrix is square
## and is otherwise invertible.  First, the function checks if the matrix has been
## computed.  If it has been computed, it obtains the result and prints, without re-doing
## the computation.  If the matrix has not been solved, it solves for the matrix inverse,
## and then sets the value with the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mat_inv <- x$getinverse()
        if(!is.null(mat_inv)) {
                message("getting cached data")
                return(mat_inv)
          
        }
        data <- x$get()
        mat_inv <- solve(data, ...)
        x$setinverse(mat_inv)
        mat_inv
}

## The following is the matrix that was for testing.  The code and values are comm-
## ented out.

## First, a 2x2 matrix
## x <- rbind(c(1, -0.25), c(-0.25, 1))
## m <- makeCacheMatrix(x)
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00

## Now to show now that it is coming from a cache
## cacheSolve(m)
## getting cached data
##       [,1]  [,2]
## [1,]  1.00 -0.25
## [2,] -0.25  1.00
