## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix creates the a list containing the following functions:
## 1. getInv - return the inverted matrix
## 2. setInv - cache the inverted matrix
## 3. get - return the current matrix
## 4. set - set the current matrix

## makeCacheMatrix creates a list containing the submitted matrix
## a value to store the inverse and the above functions

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL #solved matrix is null to start
    set <- function(y) {
        x <<- y
        s <<- NULL #solved matrix is nulled since this is a new matrix
    }
    get <- function() x
    setInv <- function(inv) s <<- inv
    getInv <- function() s
    list(set = set, get = get,
         setInv = setInv,
         getInv = getInv)
}


## cacheSolve takes the result from makeCacheMatrix
## if the matrix is new it solves it and caches the solved matrix
## once the solved matrix is cached (in s) it can return it
## rather than having to solve it again

cacheSolve <- function(x, ...) {
    mat <- x$get()
    s <- x$getInv()
    if(!is.null(s)) {   ##if we have a value for s we dont need to solve again
        message("getting cached data")
        return(s)
    }
    s <- solve(mat)
    x$setInv(s)
    s
}
