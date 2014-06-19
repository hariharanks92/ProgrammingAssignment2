## Contains two functions makeCacheMatrix and cache Solve
## makeCacheMatrix has functions with special "<<" variables
## cachesolve can use that variable and get the inverse of a matrix without needing to calculate again

## Has 4 functions inside this function which is helpful in getting the input of matrix,
## Getting the input of inverse matrix and outputs both using seperate function

makeCacheMatrix <- function(x = matrix()) {
    
    m <- NULL
    setMatrix <- function(y) {
        m <<- NULL
        x <<- y
    }
    getMatrix <- function() x
    setInverse <- function(matrixInv) m <<- matrixInv
    getInverse <- function() m
    list( setMatrix = setMatrix,
          getMatrix = getMatrix,
          setInverse = setInverse,
          getInverse = getInverse
    )
}


## Cheecks the cache of the inverse already exists
## If it doesnt, then creates the inverse using solve function
## and inputs the value by in the setInverse function of the makeCacheMatric function

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    
    ## Checks if the obtained value is NULL. If not NULL, then the value is returned, which is the inverse
    if(!is.null(m)){
        print("getting cached value")
        return(m)
    }
    
    ## If the above value is NULL, then it jumps here
    ## Solves for mInverse and returns the value
    data <- x$getMatrix()
    m <- solve(data)
    
    x$setInverse(m)
    return(m)  ## Return a matrix that is the inverse of 'x'
}
