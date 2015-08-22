## The two functions below work together to solve a matrix in
## an optimised way.
## Use makeCacheMatrix() first to make a 'special' matrix that
## can then be solved using cacheSolve(). The optimisation occurs
## if the same matrix is solved two or more times by returning
## the cached result of the first time it was solved, rather than
## solving it every time.

## Makes a new 'special' matrix.
## The special matrix exposes four functions which are used to
## get and set the matrix, as well as getting and setting the
## cached inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Solve the special matrix.
## Use the previously cached result, if it exists, otherwise
## calculate the result and cache it for next time.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        m <- x$get()
        i <- solve(m, ...)
        x$setinverse(i)
        i
}
