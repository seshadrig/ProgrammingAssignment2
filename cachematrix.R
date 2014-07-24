##this program caches the computations for the inverse of a matrix, so 
## if the contents are not changing the inverse is retrieved from the cache
## instead of computing it everytime


## This function creates a special "matrix" object that can cache its inverse.
## It has getter and setter methods to both retrieve the matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {

	 # to check if the input is of type matrix
	  if(!is.matrix(x) )
	  {
	      stop ("input must be a matrix")
	  }
	  
	  m <- NULL
	  set <- function(y) {
	    x <<- y
	    m <<- NULL
	  }
	  get <- function() x
	  setinverse <- function(inverse) m <<- inverse
	  getinverse <- function() m
	  list(set = set, get = get,
	       setinverse = setinverse,
       getinverse = getinverse)


}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         m <- x$getinverse()
	  
	  if(!is.null(m)) {
	    message("getting cached data")
	    return(m)
	  }
	  
	  data <- x$get()
	  m <- solve(data, ...)
	  x$setinverse(m)
  	  m
}
