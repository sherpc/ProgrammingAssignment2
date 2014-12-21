## Actually, I mean that this assigment implementation of memoized version of 
## solve fn isn't good. We need to create some special cache matrix, when if R 
## language possible to fast compare two matrices, we can write fn that will 
## cache values without special cache matrix. And more, we can write general
## "memoize" function, that will add cache functionality to any fn.

## On other side, I'm understand, that assigment's version is more simple 
## and more efficient in some cases. Also, if R can't compare two matrices 
## fast, more general version will work slow on big inputs.


## Assigment solution:

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
