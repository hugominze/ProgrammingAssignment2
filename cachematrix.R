## The following code creates a matrix. Its inverse will be
## stored in the cache, enabling you to save valuable time when working
## with large datasets. Good luck!

## First, the function makeCacheMatrix creates a matrix object that can cache 
## its inverse. It returns a list that stores all its functions together with 
## the associated variables in a list.

## Afte running the functions below, do not hesitate and create an example matrix
## using this code:
## my_list <- makeCacheMatrix()
## my_list$set(matrix( seq(1, 4, 1), nrow=2))
## my_list$get()

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inverse <<- inv
        getinverse <- function() inverse
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## In the next step, a second function called cacheSolve computes the inverse 
## of the matrix returned by makeCacheMatrix. 
## However, if the inverse has been already calculated, 
## then cacheSolve will retrieve the information from the cache. Try it out with:
## cacheSolve(my_list)
## cacheSolve(my_list)
## You can see that when you call the function for the first time, it will
## compute the result. When you call the same function a second time it understands
## that the inversed matrix is already stored in the first function. So, it will
## not do the computation again but get the result from the cache.

cacheSolve <- function(x,...) {
        inverse <- x$getinverse()
        if (!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        spec_matrix <- x$get()
        inverse <- solve(spec_matrix)
        x$setinverse(inverse)
        return(inverse)
        
}