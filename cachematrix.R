## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function ( x = matrix() ){
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setcache <- function(cache) m <<- cache
    getcache <- function() m
    list(set = set, get = get,
         setcache = setcache,
         getcache = getcache)
}


## Write a short comment describing this function
#This function computes the inverse of the "matrix" returned by makeCacheMatrix. 
#If the inverse has already been calculated (and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function (x, ...){
  ## Return a matrix that is the inverse of 'x'
    m <- x$getcache()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setcache(m)
    m
}
