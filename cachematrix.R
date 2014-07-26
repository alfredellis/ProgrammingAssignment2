## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# makeCacheMatrix
# This function uses closures to create a pseudo-object,
# in this case the returned list, whose methods ("set","get",
# "setinv","getinv") have persistent variables ("x" the supplied 
# matrix and "m" the cached inverse) only accessible to the
# specific instantiation of this list.

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


## Write a short comment describing this function

# cacheSolve
# This function takes a makeCacheMatrix object and
# returns the inverse for the object's underlying
# matrix.  It does this by first checking the
# object to see if a cached version of the matrix
# exists.  If the cached version exists, it 
# returns this cached version.  If the cached
# version does not exist, it calculates the
# inverse for the object's matrix and caches
# the inverse in the object before returning 
# the inverse back to the caller.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m    
}
