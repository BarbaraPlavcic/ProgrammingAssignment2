## If the contents of a matrix are not changing, it may make sense to cache
## the value of the inverse so that when we need it again, it can be looked
## up in the cache rather than recomputed. The following two functions are used 
## to cache the inverse of a matrix.

## makeCacheMatrix creates a list containg a function to set the value of the matrix,
## get the value of the matrix, set the value of the inverse of the matrix, get the
## value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv
	list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

## cacheSolve function returns the inverse of the matrix (the function assumes that the
## matrix is invertible). It first checks if the inverse has already been computed - if so
## it gets the result, otherwise it computes the inverse and sets it to cache.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
	if(!is.null(inv)) {
		message("getting cached data.")
		return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInverse(inv)
	inv
}
