## The functions makeCacheMatrix and the cacheSolve function in the script helps 
##to cache the matrix inverse operation as the operations are time consuming in 
nature

## This function is responsible to cache the inverse of the matrix 
##It has four methods get, st, getinverse and setinverse that are responsible for
##caching the matrix.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
        set <- function(y) {		##sets the matrix object 
                x <<- y
                inverse <<- NULL
        }
        get <- function() x        ##returns the matrix object
        setinverse <- function(inverseval)
		 inverse <<- inverseval ##sets the inverse of the matrix
        getinverse <- function() 
			inverse 		##returns the cahced inverse matrix
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )##list of four functions used in caching


}


## This function checks if the inverse matrix is already cached, if not
## it calls the setinverse method to cache the matrix after calculating 
##its inverse 
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x,...) {
      
m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data") 
##retreives the matrix if already exists
                return(m)
        }
        data <- x$get()
##solve method is used to calculate the inverse
        m <- solve(data)
        x$setinverse(m)
        m 
}
