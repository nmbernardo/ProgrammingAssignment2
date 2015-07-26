## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
#
# function makeCacheMatrix
# creates a special matrix data structures for cached inverse computation
makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinverse <- function(val) inv <- val
        getinverse <- function() inv
        
        list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
#function cacheSolve()
# solves the inverse and stores it in a cache. If already solved, the function will not recompute
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		
		 inv <- x$getinverse()
        
        if(!is.null(inv)) {
                return(inv)
        }
        
        inv <- solve(x$get())
        x$setinverse(inv)
        inv		
}
