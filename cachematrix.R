## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { 
mi <- NULL
set <- function(y) {
x <<- y
mi <<- NULL
}
get <- function() x
setinverse <- function(solve) mi <<- solve
getinverse <- function() mi
list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
mi <- x$getinverse()
if(!is.null(mi)) {
message("getting cached matrix")
return(mi)
}
data <- x$get()
mi <- solve(data, ...)
x$setinverse(mi)
mi
}
