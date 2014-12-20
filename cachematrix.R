## These are two helpful functions that create a matrix object that can cache its 
## inverse so that it won't be computed repeatedly.


## This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	set <- function(y){
		x <<- y
		inverse <<- NULL 
	}
	get <- function() x
	setInverse <- function(solve) inverse <<- solve
	getInverse <- function() inverse
	list (set = set, get = get, setInverse= setInverse,getInverse = getInverse )

}


## This function checks whether the inverse of a matrix is previously computed or not. 
## If computed ( and matrix has not changed), it will retrieve the inverse calculated 
## from previous function. If not, it will compute it and return it to the caller.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
        	message("getting cached data")
        	return(inverse)
        }
        data <- x$get()
        inverse <- solve(data,...)
        x$setInverse(inverse)
        inverse
}
