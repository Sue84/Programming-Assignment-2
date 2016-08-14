##Matrix inversion is usually a costly computation and there may be some benefit 
#to caching the inverse of a matrix rather than compute it repeatedly.
##belows are a pair of functions for creating a special object to 
##store a matrix and cache the inverse of a matrix.

##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inver <- NULL
        set <- function(y) {
                x <<- y
                inver <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inver <<- inverse
        getInverse <- function() inver
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned 
##by makeCacheMatrix above. If the inverse has already been calculated 
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        inver <- x$getInverse()
        if(!is.null(inver)) {
                message("getting cached data")
                return(inver)
        }
        matr <- x$get()
        inver <- solve(matr, ...)
        x$setInverse(inver)
        inver
}
