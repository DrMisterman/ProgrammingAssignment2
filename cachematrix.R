## Put comments here that give an overall description of what your
## functions do
## Return a matrix that is the inverse of 'x'

## Write a short comment describing this function
## This function creates a "special matrix" that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above.If the inverse has already been calculated (and the
## has not changed), then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
         m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
        ## Return a matrix that is the inverse of 'x'
}

## Example matrix
Mmatrix <- makeCacheMatrix(matrix(c(3, 3, 6, 10), 2, 2))
Mmatrix$get()
cacheSolve(Mmatrix)
Mmatrix$getInverse()
