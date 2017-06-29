## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse. The special "matrix" is really a list 
# containing a function to
#1. set the value of the matrix
#2. get the value of the matrix
#3. set the value of the inverse
#4. get the value of the inverse


makeCacheMatrix <- function(x = matrix()) { 
mi <- NULL   #"mi" denotes matrix inverse
set <- function(y) {  #sets the value of the matrix
x <<- y
mi <<- NULL
}
get <- function() x                                #gets the value of the matrix
setinverse <- function(inverse) mi <<- inverse     #sets the value of the inverse
getinverse <- function() mi                       #gets the vaule of the inverse
list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## The follwoing function computes the inverse of the special "matrix" returned by makeCacheMatrix above. It first checks if 
# the inverse has already been computed. If so, it gets the result and skips the computation. If not, it computes the inverse, 
#sets the value in the cache via setinverse function.  This function assumes that the matrix is always invertible.
 

cacheSolve <- function(x, ...) {        
mi <- x$getinverse()
if(!is.null(mi)) {
message("getting cached matrix")
return(mi)
}
data <- x$get()
mi <- solve(data, ...)  #inverse is computed using solve() function
x$setinverse(mi)
mi
}

#Sample Run
#> x = matrix(1:4, byrow= TRUE, nrow=2)
#> x
#     [,1] [,2]
#[1,]    1    2
#[2,]    3    4
#> y = makeCacheMatrix(x)

#> y$get()    #getting cache matrix
#     [,1] [,2]
#[1,]    1    2
#[2,]    3    4
#> 
#> cacheSolve(y) #computes inverse but no cache in the first run
#     [,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
#> 
#> cacheSolve(y)  #Retrieving inverse from the cache in the second run
#getting cached matrix
#     [,1] [,2]
#[1,] -2.0  1.0
#[2,]  1.5 -0.5
