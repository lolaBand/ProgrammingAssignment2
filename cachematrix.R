###############################################
#makeCacheMatrix: This function creates a special "matrix" object 
# that can cache its inverse.
###############################################

makeCacheMatrix <- function(m = matrix()) {
	# init the inverse cache
      i <- NULL

	# function set the value of the matrix
      setmatrix <- function(matrix) {
		# set the value
      	m <<- matrix
		# clear the cache
            i <<- NULL 
      }

	# function get the value of the matrix
      getmatrix <- function() m

	#fucntion set the value of the inverse
      setinversematrix <- function(inverse) i <<- inverse

	# function get the value of the inverse
      getinversematrix <- function() i

	# list of all functions
      list(setmatrix = setmatrix, getmatrix  = getmatrix,
             setinversematrix = setinversematrix,
             getinversematrix = getinversematrix )
}

###############################################
#cacheSolve: This function computes the inverse of the special "matrix" 
# returned by makeCacheMatrix above. If the inverse has already been calculated 
#(and the matrix has not changed), then the cachesolve should retrieve 
# the inverse from the cache.
###############################################
cacheSolve<- function(x, ...) {
	#get the matrix (inverse of x)
      i <- x$getinversematrix()

	# if inverse matrix is calculated, return the value
      if(!is.null(i)) {
      	message("getting cached data")
            return(i)
      }

	# get the matrix
      matrix <- x$getmatrix()

	# get the inverse
      i <- solve(matrix, ...)

	#set the value of inverse of the matrix into the cache
      x$setinversematrix (i)

	# return the inverse
      i
}
