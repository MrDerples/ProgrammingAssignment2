# -----
# This file contains definitions for two functions, 'makeCacheMatrix' and 
# 'cacheSolve'. These functions constitute the deliverable for Programming 
# Assignment 2.
#
# -nj 2014-9-18
# -----


# Creates a special matrix object capable of caching its inverse. Defines 
# child functions for getting/setting matrix and cached inverse.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
    
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	
	get <- function() x
	
	setInverse <- function(inverse) inv <<- inverse
	
	getInverse <- function() inv
	
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


# Solves matrix inverse if not already computed and cached. Returns cached 
# value without re-computing if cached value exists. Assumes matrix is not 
# singular.

cacheSolve <- function(x, ...) {
	inv <- x$getInverse()
	
    if( !is.null(inv) ) {
		## Inverse IS already cached
		message("getting cached data")
		return(inv)
	}
	
	## Inverse NOT already cached
	data <- x$get()
	inv <- solve(data, ...)
	x$setInverse(inv)
	inv
}