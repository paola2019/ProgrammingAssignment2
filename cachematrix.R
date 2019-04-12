## This function creates a special “matrix” object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    n <- NULL
    set <- function(y){
            x <<- y
            n <<- NULL
    }
    get <- function() x
    setInv <- function(solveMatrix) n <<- solveMatrix
    getInverse <- function() n
    list(set = set, get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

## This function calculates the inverse of the special “matrix” from the function
## makeCacheMatrix. It first checks if the inverse has already been calculated. If so,
## it gets the inverse from the cache and skips the computation.

cacheSolve <- function(x, ...) {
        n <- x$getInverse()
        if(!is.null(n)){
                message("getting cached data")
                return(n)
        }
        data <- x$get()
        n <- solve(data, ...)
        x$setInverse(n)
        n
}
