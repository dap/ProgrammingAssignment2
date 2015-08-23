## cachematrix.R - matrix inversion, with cached result to speed subsequent access
##		Usage:
##			foo <- makeCacheMatrix()
##			foo$set(matrix(rnorm(9),3,3))
##			cacheSolve(foo)

## makeCacheMatrix - create an object, which uses closed-over
## private members to handle pseudo-memoization of matrix inversion
##		params:
##      	x - matrix to be inverted (optional; may be set later use set())
##			... - additional parameters to be passed on to solve()
##		result:
##			inverted matrix
makeCacheMatrix <- function(x = matrix()) {
	ix <- NULL

	set <- function(y) {
		x <<- y
		ix <<- NULL
	}
	get <- function() {
		x	
	}
	setinverse <- function(inverse) {
		ix <<- inverse
	}
	getinverse <- function() {
		ix
	}

	# Return object with members for manipulating matrix in closure
	list(
		set = set,
		get = get,
		setinverse = setinverse,
		getinverse = getinverse
	)
}


## cacheSolve - retrieve inverse of a matrix, caching result
##		params:
##      	x - an object as returned by makeCacheMatrix
##			... - additional parameters to be passed on to solve()
##		result:
##			inverted matrix
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	ix <- x$getinverse()
	if(!is.null(ix)) {
		message("getting cached data")
		return(ix)
	}
	data <- x$get()
	ix <- solve(data, ...)
	x$setinverse(ix)
	ix
}
