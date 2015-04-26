## Put comments here that give an overall description of what your
## functions do

## makes a matrix object that caches the inverse.  Matrix is defined in x, which will be assigned before calling makeCacheMatrix and must be a square matrix e.g. x = rbind(c(1, -.5), c(-.5, 1)) to create 2 rows and 2 columns

## to test this perform the following
## x = rbind(c(1, -.5), c(-.5, 1)) 
## z = makeCacheMatrix(x)
## cacheSolve(z)
## cacheSolve(z)
## the second cacheSolve(z) should show the same matrix, but with a "getting cached data." message displayed.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	# replace the setmean and getmean functions with setinverse and getinverse
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	# create a list of namevalue pairs
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## computes the inverse of the matrix returned by makeCacheMatrix or retrieves the inverse from cache if it already exists

cacheSolve <- function(x=1, ...) {
	m <- x$getinverse()
	if(!is.null(m)) {
		message("getting cached data.")
	return(m)
	}
	data <- x$get()
	# replace mean with solve
	m <- solve(data)
	x$setinverse(m)
    m
}
