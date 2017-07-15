

## This function create an R object that is a matrix and can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y)
	{
	  x<<-y
	  inv<<-NULL
	}
	  get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse ,
             getinverse = getinverse )
}


## This function returns the inverse of matrix x if is cached, otherwise
##calculates the inverse and caches it


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse )
        inverse 
}
