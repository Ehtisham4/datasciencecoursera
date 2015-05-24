## The makeCacheMatric function creates a special matrix and stores its inverse

## This function creates a special matrix and stores its inverse

makeCacheMatrix <- function(x = matrix()) {
			inv = NULL
			set<- function(y){
				x <<- y
				inv <<- NULL
			}
			get <- function() x
        		setinverse <- function(inverse) inv <<- inverse
        		getinverse <- function() inv
        		list(set = set, get = get,setinverse = setinverse,
             	getinverse = getinverse)
}

## This function clculates the inverse if matrix is changed, otherwise it retrieves the inverse from cache.already calculated

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
       inv <- solve(data)(
        x$setinverse(inv)
        inv
}

inv