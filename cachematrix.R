## These functions are designed to help cache the inverse of a matrix,
## so that it's not necessary to run time-costly calculations every time
## this value is needed

##  This makeCacheMatrix function creates a special
## "matrix" object that can cache the inverse of an invertible matrix

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        ## set the value of inverse matrix 'i' to NULL as default
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        ##function 'set' will restore inverse value to NULL if value
        ## of matrix x is changed
        get <- function() x
        ##function 'get' returns the value of x
        setinverse <- function(solve) i <<- solve
        ##setinverse allows you to assign a value to inverse matrix 'i'
        getinverse <- function() i
        ##function 'getinverse' returns the value of inverse matrix 'i'
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then 
## cacheSolve should retrieve the inverse from the cache.

cachesolve <- function(x, ...) {
        i <- x$getinverse()
        ##We set the value of i as the inverse of x, if this has 
        ##been cached previously
                if(!is.null(i)) {
                        message("getting cached data")
                        return(i)
                }
        ##If i is not a null matrix, we return its value
        ##Otherwise we use the function solve to calculate i, the
        ##inverse of x and return this value
                data <- x$get()
                i <- solve(data, ...)
                x$setinverse(i)
                i
}

