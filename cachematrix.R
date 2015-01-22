## The functions defined in this script allow the user to cache the inverse of a matrix
## saving time and computation costs, rather than compute it repeatedly 

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	reverse <- NULL
	set <- function(y) {
			x <<- y
			reverse <<- NULL
	}
	get <- function() x
	setreverse <- function(r) reverse <<- r
	getreverse <- function() reverse
	list(set = set, get = get,
		 setreverse = setreverse,
		 getreverse = getreverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cacheSolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		reverse <- x$getreverse()
        ## In this control structure calculates the reverse if there is
		## it has not been calculated previously, is not in cache,
		if(!is.null(reverse)) {
                message("getting cached data")
                return(reverse)
        }
        data <- x$get()
        reverse <- solve(data, ...)
        x$setreverse(reverse)
        reverse
}