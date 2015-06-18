## Functions Description
## makeCacheMatrix : This function creates a special "list" which contains
## 			  functions to 1)get/set the matrix whose inverse is to be
##			  computed 2)get/set the inverse of the matrix
##cacheSolve : This function computes the inverse of matrix set in function 
##		   makeCacheMatrix
##--------------------------------------------------------------------------

## This function takes a matrix object as input and creates a special "list"
## of functions 

makeCacheMatrix <- function(x = matrix()) {

## Set inverse of matrix equal to the matrix
i <- x

set <- function(y) {
	x <<- y
	i <<- y
	}
get <- function() x
setinv <- function(inv = matrix()) i <<- inv
getinv <- function() i

list (set = set, get = get, getinv = getinv, setinv = setinv)
}

##---------------------------------------------------------------------------
## This function first checks if cached inverse of matrix and the matrix are
## are same. If yes, it computes the inverse of matrix and returns the same
## otherwise it returns the cached inverse


cacheSolve <- function(x, ...) {

i <- x$getinv()
data <- x$get()

## Checks if cached inverse is same as that of matrix
if (!identical(data,i)) {
	message("getting cached data")
	return(i)
}
message("Computing...")
i <- solve(data,...)
x$setinv(i)
## Return a matrix that is the inverse of 'x' after computing
i
       
}