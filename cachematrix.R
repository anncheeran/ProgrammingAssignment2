## The following pair of functions cache the inverse of a matrix
## Assume : Matrix passed to the function is always a square matrix

## Makes a special matrix object that will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
		## <<- operator is used to  assign a value to an object
		## in a different environment.
	m <- NULL
     	set <- function(y) {
              x <<- y
              m <<- NULL
     	}
     	get <- function() x
	setsolve <- function(solve) m <<- solve
	getsolve <- function() m
	list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
   		## Return a matrix that is the inverse of 'x'
 	m <- x$getsolve()
	
## check if the matrix is invertible
	f1 <-function(m) class(try(solve(m),silent=T))=="matrix"

## if not invertible
	if (f1(x$get())== FALSE){
		return("Matrix passed is singular/not invertible")
	}

## if invertible, return inverse from cache if calculated before
      if(!is.null(m)) {
                message("getting cached data")
                return(m)
      }

## if not calculated before, then calculate inverse and store in cache 
      data <- x$get()
      m <- solve(data, ...)
      x$setsolve(m)
      m
}
