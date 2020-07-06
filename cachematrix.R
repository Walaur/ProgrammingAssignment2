## Calculating the inverse of a matrix through caching:
## Doing simple calculations do not take a lot time to compute,
## however when the number of calculations grow it is better to
## cache the results instead of making the computation every single time.

## This function generates the cache of an inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv_matrix <- NULL
        set <- function(y) {
                x <<- y
                inv_matrix <<- NULL
        }
        get <- function() x
        setInvMatrix <- function(inverse) inv_matrix <<- inverse
        getInvMatrix <- function() inv_matrix
        list(set = set, get = get,
             setInvMatrix = setInvMatrix,
             getInvMatrix = getInvMatrix)
}

## This function creates a matrix called makeChacheMatrix where
## its inverse is computed and stored on the cache. If the inverse
## has already been calculated, instead of computing the inverse again
## the function retrieves the values of the matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_matrix <- x$getInvMatrix()
        if(!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        }
        data <- x$get()
        inv_matrix <- solve(data, ...)
        x$setInvMatrix(inv_matrix)
        inv_matrix
}