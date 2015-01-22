## The function is created identicaly to the example makeVector from the
## assigngment page
## Actually contains list of setters and getters for original and inverted
## matrix
makeCacheMatrix <- function(x = matrix()) {
	## Initializing inverted matrix
	inv_matrix <- NULL
	## Initializing both matrix in a standalone environment
	set <- function(y) {
		x <<- y
		inv_matrix <<- NULL
	}
	## Getter for original matrix
	get <- function() x
	## Setter for inverted matrix
	setInverse <- function(inverse) inv_matrix <<- inverse
	## Getter for inverted matrix
	getInverse <- function() inv_matrix
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## The function checks if inverted matrix has already been prepared by checking
## the cache. If the matrix is there, calculation is not necessary, it's just
## set and returned. Otherwise, it is computed and placed in the cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inv_matrix <- x$getInverse()
	## Checking if the matrix is already there
	if(!is.null(inv_matrix)) {
		message("The matrix has already been cached, getting...")
		return(inv_matrix)
	}
	## Inverting the matrix
	inv_matrix<-x$setInverse(solve(x$get()))
	inv_matrix
}
