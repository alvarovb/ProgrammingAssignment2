## It creates a special matrix, with the following methods:
## set: Set the value of the matrix
## get: returns the matrix
## setInverse: Set the inverse matrix to be cached
## getInverse: Returns the chached matrix
## Example of how to run it: makeCacheMatrix(runif(n^2),n)
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
   
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    
    get <- function() x
    setInverse <- function(mInverse) inverse <<- mInverse
    getInverse <- function() inverse
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}

## This method return the inverse of an special matrix, returned by makeCacheMatrix 
## It verify first that the special matrix has cached the inverse of it, 
## if it does, then  returns the cached inverse matrix, otherwise it  calculates 
## the  inverse matrix and cached it in the special matrix.
## Example of how to run it:
## mt <- makeCacheMatrix(runif(n^2),n)
## ch <- cacheSolve(mt)
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached inverse matrix")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setInverse(inverse)
    inverse
}
