## makeCacheMatrix creates a special matrix that is able to cache the inverse of the matrix
## cacheSolve makes used o the cache inverse matrix; if the inverse is available, then return it immediately without calculation.
## Otherwise calculate the inverse of the matrix, cache it before returning the inverse


## the inverse matrix is set to NULL as default
## there is also the getinverse and setinverse methods to get and set the inverse matrix respectively

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}
	get <- function() x
	setinverse <- function(im) inverse <<- im
	getinverse <- function() inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse= getinverse)
}


## cacheSolve computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve will retrieve the inverse from the cache
## Otherwise the solve() method will be used to calculate and cache the inverse matrix before returning it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if( !is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	cur_matrix <- x$get()
	inverse <- solve(cur_matrix, ...)
	x$setinverse(inverse)
	inverse
}
