## makeCacheMatrix will create special memoized version of matrix.
## cacheSolve will solve memoized matrix, trying to get cache result if called 
## more than one time.

## Returns special memoized version of matrix. 
## Needed for cacheSolve function (see below).

makeCacheMatrix <- function(x = matrix()) {
	solution <- NULL
	set <- function(y) {
		x <<- y
		solution <<- NULL
	}
	get <- function() x
	setsolution <- function(mean) solution <<- mean
	getsolution <- function() solution
	list(set = set, 
		 get = get,
		 setsolution = setsolution,
		 getsolution = getsolution)
}


## Solve special memoized version of matrix. 
## Function trying to extract cache value, and only if cache value is NULL it
## will actually solve matrix (and store result in cache).

cacheSolve <- function(x) {
	solution <- x$getsolution()
	if(!is.null(solution)) {
		message("getting cached data")
		return(solution)
	}
	data <- x$get()
	solution <- solve(data)
	x$setsolution(solution)
	solution
}
