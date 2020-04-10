## Creates a special "vector" containing a function that:
# sets+gets the value of the vector
# sets+gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) s<<-inverse
        getinverse <- function() s
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Calculates the inverse of above "vector".
## If this has already been calculated, it gets it from cache.
## Otherwise it calculates the inverse of matrix 'x', puts it in cache.
# Returns a matrix that is the inverse of 'x'.

cacheSolve <- function(x, ...) {
        s <- x$getinverse()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        mat <- x$get()
        s<-solve(mat, ...)
        x$setinverse(s)
        s

}
