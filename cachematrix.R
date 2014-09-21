## Functions to solve linear equations using a cached matrix inverse.
## To use this functionality, wrap a matrix in a Cache Matrix:
##      cm <- makeCacheMatrix(m)    // m is a matrix
## then, when the inverse of m is needed, invoke cacheSolve:
##      inv <- cacheSolve(cm)
##
## to solve the linear system m %*% x = b, add the argument b
## to the cacheSolve invocation:
##      inv <- cacheSolve(cm, b)

# This function creates a special "matrix" object
# that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    get <- function() {
        x
    }
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    getinverse <- function() {
        inv
    }
    setinverse <- function(inverse) {
        inv <<- inverse
    }
    
    list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


# This function implements 'solve' for the special "matrix" returned by
# `makeCacheMatrix` above. If the inverse has already been calculated (and the
# matrix has not changed), then `cacheSolve` uses the inverse from the cache.
#
# Note that the argument 'b' is not explicitly mentioned in the assignment.
# However, the name 'solve' implies solving a (linear) system of equations.
cacheSolve <- function(x, b = NULL) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if (is.null(inv)) {
        m <- x$get()
        inv <- solve(m)
        x$setinverse(inv)
    }
    
    args <- list(...)
    if (!is.null(args$b)) {
        inv %*% args$b
    } else {
        inv
    }
}
