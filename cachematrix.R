## makeCacheMatrix accepts an inversable matrix.
## It provides methods for getting and setting the matrix
## and the inverse. When the inverse is set it is cached
## until it is recalculated or the matrix is updated.
## cacheSolve returns the inverse of the matrix in makeCacheMatrix
## by either returning a cached version or calculating it when
## no cached version exists. The newly calculated inverse is cached.

## Takes a matrix and allows for an inverse to be cached in memory
## Lists methods for getting and setting the matrix and inverse

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	
	#once instanciated, overwrite original matrix with new one
	#also clear out the cached inverse
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	#return the matrix itself
	get <- function() x
	
	#set the scoped inv variable to the supplied inverse
	setinv <- function(inverse) inv <<- inverse
	
	#return the inv
	getinv <- function() inv
	
	#return the functions as a list
	list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Accepts a "matrix-like" object which lists methods for
## getting and setting the matrix and its (cached) inverse.
## Calculates an inverse when none is available,
## otherwise returns a cached copy.

cacheSolve <- function(x, ...) {
	#get the inverse if it has already been calculated
	inv <- x$getinv()
	if(!is.null(inv)) {
		message("inverse returned from cache")
		return(inv)
	}
	
	message("inverse calculated for the first time")
	
	#because there was no cached inverse, get the data actual matrix...
	mtrx <- x$get()
	#... calculate the inverse...
	inv <- solve(mtrx)
	#...cache the inverse, then return it
	x$setinv(inv)
	inv
}
