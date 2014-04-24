## =============================================================================
## Function:	makeCacheMatrix <- function(x = matrix())
## -----------------------------------------------------------------------------
## Description:	Function that results in a list of functions
##			for storing and retrieving a matrix and its inverse
##			resulting a matrix "object" with those functions
##
## Details: (with apologies for verbosity, trying to understand it all myself)
	# 1. Declare a function that takes a matrix x as an argument
	# 2. Initalize variable matrix m as null 
	# 3. Create a set "method" that effectively resets m to null and persists
	#	(caches) both m and x.  Used to start over, but not employed in
	#	this particular assignment.
	# 4. Create a get "method" that simply reads the input matrix x
	# 5. Create a setinverse "method" that inverts a square matrix 
	#	using solve() {from base package}
	# 6. Create a getinverse "method" that tries to simply read the
	#	inverse matrix m
	# 6. Returns a list of these functions as a factory of sorts to
	#	make "cacheable" matrices (lists) which have these functions

## Arguments:	matrix 
##
## Returns: 	List of functions: set, get, setinvert, getinvert
##
## Usage:		
	# 	create a test matrix
	#		z <- matrix(1:4,2)
	#	create an "augmented" matrix/list
	#		zplus <- makeCacheMatrix(z)
	#	this could be shortened to
	#		zplus <- makeCacheMatrix(matrix(1:4,2))
	#	but it is nice to be able to test the result using
	#		z %*% cacheSolve(zplus)
## =============================================================================
makeCacheMatrix <- function(x = matrix()) {
	
	# Initialize matrix m (to hold the matrix inversion of x)
	m <- NULL
	
	# Define "set" function for re-initializing m and storing x
	# Reset m as null, and set (persist) the matrix x and inverse mx 
	# in the parent environment, both using the '<<-' assignment
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	
	# define "get" function for getting (reading in) the matrix x
	get <- function() x
	
	# define "setinverse" function to invert the matrix and caching it
	# per hint on forums, assume square matrix and invert using solve()
	
	# the <<- assignment performs the caching by persisting the variable
	#   m in the parent environment of the function
	setinverse <- function(solve) m <<- solve
	
	# define the "getinvert" function, which simply tries to retrieve m
	# thus returning either a cached matrix or NULL
	getinverse <- function() m
	
	# return a list of the functions
	list( set = set, 
		get = get,
		setinverse = setinverse,
		getinverse = getinverse)
 
}

## =============================================================================
## Function:	cacheSolve <- function(x, ...) 
## -----------------------------------------------------------------------------
## Description:	Return the inverse of a square matrix, either from a cached
##			result, or if one is not available, computing and caching 
##			the inverse
##
## Details: (more verbosity)
	# 1. Declare a function that takes a matrix x as an argument
	# 2. See if the inverted matrix is already available (cached) using
	#	getinverse() function from makeCacheMatrix
	# 3. If no cached result exists, 
	#	a. Read the original matrix from parent environment 
	#		using get() function from makeCacheMatrix
	#	b. Compute the inverse
	#	c. Cache the inverse in the parent environment using
	#		setinverse() function from makeCacheMatrix 
	# 4. Return the inverted matrix 

## Arguments:	"matrix" (list) created by makeCacheMatrix()
##
## Returns: 	Matrix inverse of argument
##
## Usage:		See above makeCacheMatrix()
## =============================================================================
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of matrix x
        
        # try getting the cached inverse (A), if it exists
        m <- x$getinverse()
        
        # if it does exist (not null), read it and return that matrix
        # and exit function
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        # if it is null (not cached), calculate the inverse
        # first, read the original matrix (passed as argument) 
        data <- x$get()
        
        # invert
        m <- solve(data)
        
        # cache the result for future use
        x$setinverse(m)
        
        # return the inverse
        m
}       
