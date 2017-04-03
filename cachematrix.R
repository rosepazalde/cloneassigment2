## The makeCacheMatrix creates a special object that has functions to cache the inverse of the input matrix
## The cacheSolve looks to see if the inverse matrix is already in cache memory, and if not calculates the inverse and returns solution

## This function takes an input matrix, and creates an object with the input matrix and the functions to store it to cached memory
## Initialize variable to hold the inverse matrix (invmat) to NULL
## define a function set to create space in cached memory for the inverse matrix
## define a function to get the input matrix,  "get"
## define a function to setinv which puts the inverse matrix into cached memory
## define a function getinv which returns whats in cached memory (invmat)

makeCacheMatrix <- function(x = matrix()) {
invmat <- NULL
set <- function(y) {
  x <<- y
  invmat <<- NULL
  }
get <- function() x
setinv <- function(inverse) invmat <<- inverse
getinv <- function() invmat
list(set = set, get = get, setinv = setinv, getinv = getinv)
}


##This function checks to see if the inverse matrix is 
##already in cached memory, and if so returns,
## if not calculates the inverse and puts in cached memory


cacheSolve <- function(x, ...) {
       #get the value that is in cached memory
#and if it is not null, return with the value
invmat <- x$getinv()
if(!is.null(invmat)) {
	message("getting cached inverse matrix")
	return(invmat)
}
 ## Return a matrix that is the inverse of 'x'
data <- x$get()
invmat <- solve(data, ...)
x$setinv(invmat)
invmat

}
