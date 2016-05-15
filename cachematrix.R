## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It is really a list containing a function to a list of functions to get and set the matrix, and get and set its inverse.
## The special matrix created this ways is given to the cacheSolve function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
     set <- function(y) {
         x <<- y
         m <<- NULL
              }
     get <- function() x
     setinv <- function(inv) m <<- inv
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
 }  
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
## If not, it calculates the inverse and the inverse adds it to cache.
cacheSolve <- function(x) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}

