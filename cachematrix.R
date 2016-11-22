## youngdavidlee/ProgrammingAssignment2
## makeCacheMatrix Funtion returns matrix & stores inverse of the matrix in memory

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) i <<- solve
        getInverse <- function() i
        list (set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)


## cacheSolve Fuction returns inverse of the matrix. 


cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)
        i
        ## cacheSolve Function return a matrix that is the Inverse of 'x'
        ## Example: a <- matrix(c(2,4,1,8), nrow = 2, ncol = 2)
        ## a returns 2 x 2 matrix which contains 2,4,1,8
        ## 1/a returns 2 x 2 matrix which contains 0.50, 0.25, 1.000, 0.125
}

