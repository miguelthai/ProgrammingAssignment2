## This will demonstrate the ability to create an inverse of a matrix
## and then cache that result as needed

## The following function creates a matrix, creates its inverse and sets it into cache

makeCacheMatrix <- function(x = matrix()) {

        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        
        get <- function () x
        setInverse <- function(solveMatrix) Inv <<- solveMatrix
        getInverse <- function() Inv
        
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



## This function computes the inverse of the matrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        Inv <- x$getInverse()
        if(!is.null(Inv)) {
                message("getting cached data")
                return(Inv)
                
        }
        data <- x$get()
        Inv <- solve(data)
        x$setInverse(Inv)
        Inv
}
