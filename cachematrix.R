## these functions inverse matrix

MakeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmatrix <- function(inverse) (m <<- inverse)
        getmatrix <- function() (m)
        list(set = set, get = get,
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}
## the function cachesolve prevents the NULL for matrix inverse correctly
CacheSolve <- function(x, ...) {
        m <- x$getmatrix()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setmatrix(m)
        m
}

## Example
matrixex <- MakeCacheMatrix(matrix(1:4, 2, 2)) ##create the matrix
matrixex$get() ## get the matrix
matrixex$getmatrix() ## try to inverse but gets null
CacheSolve(matrixex)
CacheSolve(matrixex) ## this two cast gets cached data
matrixex$getmatrix() ##inverses the matrix