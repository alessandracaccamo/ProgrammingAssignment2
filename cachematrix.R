## Function in order to create a matrix object (ex) that can cache its inverse.

makeCacheMatrix <- function(ex = matrix()) {
    inv_ex <- NULL
    set <- function(x) {
        ex <<- x
        inv_ex <<- NULL
    }
    get <- function() ex
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv_ex
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function to compute the inverse of the matrix put in the function above.

cacheSolve <- function(ex, ...) {
    inv <- ex$getInverse()
    if (!is.null(inv)) {
        message("data in cache")
        return(inv_ex)
    }
    matrix <- ex$get()
    inv_ex <- solve(matrix, ...)
    ex$setInverse(inv_ex)
    inv_ex
}