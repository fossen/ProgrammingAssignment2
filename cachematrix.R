## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  ## restore inverse value to null
        set <- function(y) {
                x <<- y   ##<<- operator expands scope for matrix assignment to parent environment
                inv <<- NULL   ## to prevent out of date inverse matrix value if set is called
        }
        get <- function() {
                x
        }
        setinverse <- function(mat) {
                inv <<- solve(mat)
        }
        getinverse <- function() {
                inv
        }
        list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()   # 1st check if inverse exists in cache
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        matx <- x$get()     # default if inverse isn't in cache
        inv <- solve(matx)
        x$setinverse(inv)
        inv
}
