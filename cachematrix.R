
##Our aim in this experiment is to write a pair of functions, namely,
##"makeCacheMatrix" and "cacheSolve" that cache the inverse of a matrix


## makeCacheMatrix is a function which creates a special "matrix" object that can
## cache its inverse for the input (which is an invertible square matrix)

makeCacheMatrix <- function(x = matrix()) {			  ##Default argument is matrix to cache the inverse
  i <- NULL							                        	##Initialise inverse i to NULL					
  set <- function(y = matrix()) {				          ##Set function is defined to set the value of the matrix	 
    x <<- y
    i <<- NULL						                        ##Inverse i of the matrix is set to NULL once more
  }
  get <- function() { x }					                ##Get function return the matrix itself
  setinv <- function(inverse) { i <<- inverse }  	##Setinv function sets the inverse that is calculated to inv
  getinv <- function() { i }				            	##Getinv function retrieves the inverse value
  list(set = set, get = get,				              ##Add the functions to a list
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve is a function which computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve the 
## inverse from the cache

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached result")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}	



