## The functions will solve the inverse of the input matrix. If the 
## solution was calculated before, the same calculation will not be
## proceeded. Result from previous calculation will be returned.

## makeCacheMatrix() funcion gets the input matrix and stores the 
## inverse matrix calculated by cacheSolve() function.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
                get = get,
                setinverse = setinverse,
                getinverse = getinverse)
        
}


## cacheSolve() function will judge if the matrix has been solved before.
## If the matrix has been solved before, the function will return the 
## results calculated before, rather than doing another round of 
## calculation.
## If the matrix has never been solved, the function will solve the 
## inverse matrix.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
        
}
