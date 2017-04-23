## Sometimes we need an R function which is able to cache potentially time-comsuming computations.
## We'll take advantage of the scoping rules of the R to accomplish this.

## makeCacheMatrix function creates a special "vector", which is a list of function to 
## set the value of the matrix, get the value of the matrix, set the value of the inverse
## of the matrix and get the value of the inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
       m    <- NULL
	   set  <- function(y){
	           x <<- y
			   m <<- NULL
	   }
	   get  <- function () x
	   setInverse <- function(Inverse){ m <<- Inverse }
	   getInverse <- function() m
	   list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)	   
}


## cacheSolve calculates the inverse of the special "vector" created with "makeCacheMatrix".

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
		if(!is.null(m)){                         ## first check if the inverse has been calculated
		      message("getting cached inverse")  ## if so, get the inverse from the cache
		      return(m)
		}
		data <- x$get()                   
		m <- solve(data,...)      ## otherwise, calculate the inverse of the matrix
		x$setInverse(m)           ## set the value of inverse in the cache 
		m                         ## Return a matrix that is the inverse of 'x'
		
}
