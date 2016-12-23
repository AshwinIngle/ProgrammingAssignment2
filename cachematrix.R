## Put comments here that give an overall description of what your
## functions do
  ## The following pair of functions work in cordination to create a squrae invertible matrix 
  ## This inverse of square matrix is stored in cache environment for future use

## Write a short comment describing this function
  ## makeCacheMatrix creates a matrix object that caches its inverse and returns a list of functions
  ## used by the following cacheSolve to get or set the inverted matrix in cache	


makeCacheMatrix <- function(x = matrix()) {
	##store cached value
	##initialize to NULL
	 cache <- NULL
	
	##creating matrix in working environment
	 set <- function(y) {
		   x <<- y
		   cache <<- NULL
		  }
  
   ##get matrix value
    get <- function() x	
   
   ##inverse the matrix and store in cache
    setMatrix <- function(inverse) cache<<- inverse
 	
   ##get inverted matrix from cache
    getInverse <- function() cache
   
   ##return created fnuctions working environment  	
    list(set = set, get = get , setMatrix = setMatrix , getInverse = getInverse)
  }

 
## Write a short comment describing this function
  ##cacheSolve calculates inverse of matrix created in makeCacheMatrix
  ##if invert already is created and exists in cache then it is retrieved else
  ##if invert does not exists in cache, it is created in working environment
  ##and value is stored in cache 


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

    ##attempting to get inverse if any stored in cache
	cache <- x$getInverse()    
    
    ##return inverse of matrix from cache if it exists
	if(!is.null(cache)) {    	
	      message("getting cached data")
		return(cache)
	} 

    ##create matrix as it does not exists in cache
      matrix <- x$get()
	cache <- solve(matrix, ...)
	x$setMatrix(cache)
	return(cache)
}
