## These functions take a square matrix, create a list of functions used to 
## manipulate the matrix and calculate its inverse, and cache the value of the inverse
## so that it does not need to be recalculated at a later point, which can be 
## computationally intensive if the matrix is large.


## The first function creates a list containing four functions allowing a 
## user to manipulate a matrix. The first function (setmatrix()) allows the
## user to set the value of the matrix. The second (getmatrix()) retrieves the
## value of the matrix. The third (setinverse()) sets the value of the inverse
## of the matrix, and the fourth (getinverse()) retrieves the value of the
## inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	setmatrix <- function(y) { ## allows input of a new matrix (y), resets m to NULL
		x <<- y
		m <<- NULL
	}
	getmatrix <- function() x ## returns the matrix passed to makeCacheMatrix
	setinverse <- function(inverse) m <<- inverse ## sets m equal to inverse in parent environment
	getinverse <- function() m ## returns m
	list(setmatrix = setmatrix,
		getmatrix = getmatrix,
		setinverse = setinverse,
		getinverse = getinverse) ## makeCacheMatrix returns a list containing the four functions
}


## The second function checks to see whether there is already a cached value for
## the inverse of the matrix using the getinverse() function. If there is, it does
## not compute the inverse and returns the cached value. If not, then it calculates
## the inverse of the matrix and returns it, and also sets the inverse of the matrix
## in the cache using the setinverse() function.

cacheSolve <- function(x, ...) {
    m <- x$getinverse() ## sets m equal to m from makeCacheMatrix()
    if(!is.null(m)) {
    	message("getting cached data")
    	return(m) ## if m is not NULL then return m - the inverse matrix already cached
    }
    mat <- x$getmatrix() ## if m is NULL then retrieve inputted matrix from getmatrix()
    m <- solve(mat, ...) ## then calculate inverse and assign value to m
    x$setinverse(m) ## then use setinverse() function to assign inverse value to m within makeCacheMatrix().
    m ## will no longer be NULL and will be cached as such
}