##Functions to find inverse of a matrix, with optimization using
##caching

## The function makeCacheMatrix creates a special "matrix", which 
## is a list containing functions to
## 1. Set the values of the matrix
## 2. Get the values of the matrix
## 3. Set the value of the inverse
## 4. Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        set <<- y 
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function caheSolve calculates inverse of a special "matrix"
## created with the previous function. However it first checks to
## see if the inverse has been already found. If so, it gets the
## inverse from the cache and skips the computation. Otherwise, it
## finds the inverse and sets the value of the inverse in the 
## cache via the setinv function

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    if(!is.null(inv)) {
        message("Getting cached data..")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setinv(inv)
    inv  
}
