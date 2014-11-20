## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve will 
## retrieve the inverse from the cache.

## makeCacheMatrix accepts any matrix as input
## Returns a list of functions that can be performed on that matrix.
## Without input matrix it will just initialise the list,
## With input it will assign that matrix to the outer variable x.
## Functions defined within this function is
## set - set the value of the matrix
## get - get the value of the matrix
## setinverse - assign the value of the inverse to the variable m
## getinverse - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
		 m <- NULL
         set <- function(y) {
				# set the "outer" variable x to the input of this function
                 x <<- y
                 m <<- NULL		 
         }
         get <- function() {
				# Return whatever we have in x
				x				
		}
         setinverse <- function(solve) {
		 	    # Set the value of the "outer" parameter m to the input of this function
				m <<- solve
		 }
         getinverse <- function() {
		        # Return whatever we have in m
				m
		}
		# Return a list with the accessible functions in this function.
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
 }
## Return a matrix that is the inverse of the matrix that was assigned in the makeCacheMatrix function
## cacheSolve function computes the inverse of the special "matrix" assigned 
## by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then cacheSolve will 
## retrieve the inverse from the cache.
## Returns:
## - a matrix that is the inverse of the matrix that was assigned in the makeCacheMatrix function
## - an error if the matrix is not able to inverse.
## - a matrix with NA if the input to makeCacheMatrix was empty.)
cacheSolve <- function(x, ...) {        
		# Check if m is set
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
		# get the value of the matrix into data
        data <- x$get()
		# Set m to the inverse of the matrix
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

