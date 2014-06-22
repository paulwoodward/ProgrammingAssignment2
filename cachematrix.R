
# Creates a wrapper for a matrix and verifies that it is square
# - return: A wrapper for the matrix x
makeCacheMatrix <- function(x = matrix()) {
    x <- ensureSquareMatrix(x)
    i <- NULL
    
    set <- function(y) {
        x <<- ensureSquareMatrix(y)
        inv <<- NULL
    }
    
    get <- function() x
    setInverse <- function(inv) i <<- inv
    getInverse <- function() i
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Validates that x is a square matrix
# - return: x
ensureSquareMatrix <- function (x) {
    if(!is.matrix(x)) {
        stop("x must be a matrix")
    }
    
    if(dim(x)[1] != dim(x)[2]) {
        stop("Cache matrix requires a square matrix")
    }
    x
}

# Gets and caches the inverse of the given matrix
# - return: The inverse of x
cacheSolve <- function(x, ...) {
    
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Returning cached inverse matrix")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
