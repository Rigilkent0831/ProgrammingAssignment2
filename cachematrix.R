## These two functions are used to create a "matrix" object, return its 
## inverse, and then cache it for later use 

## The "makeCacheMatrix" function will create a "matrix" object which contains
## 2 pairs of "mutator" and "accessor" for Matrix and it's inverse respectively

makeCacheMatrix <- function(x = matrix()) {
        x.inv <- NULL
# Mutator and Accessor of matrix object
# setmatrix is used to assign a new matrix "x" for calculation and 
# also reset the cached inverse of matrix "x.inv"
        setmatrix <- function(y){
                x <<- y
                x.inv <<- NULL       
        }
#retrieve the latest matrix used for the calculation        
        getmatrix <- function() x
        
# Mutator and Accessor of inverse of the matrix object
# setinverse is used to create a function to be use by "cacheSolve" function 
# to assign the result of inverse matrix calculation to "x.inv"
        setinverse <- function(inverse) x.inv <<- inverse
        getinverse <- function() x.inv
# generate a list of functions consited of 2 pairs of Mutators and Accessors
# for matrix and its matrix inverse.
        list(setmatrix = setmatrix, getmatrix = getmatrix,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The "cacheSolve" function will return the inverse of a "matrix" object
## created by "makeCacheMatrix" function. It will retrieve an already 
## cached inverse of the "matrix" if any, or it will calculate and cache it.

cacheSolve <- function(x, ...) {
        x.inv <- x$getinverse()
        if(!is.null(x.inv)) {
                message("getting cached data")
                return(x.inv)
        }
        
        ## Return a matrix that is the inverse of 'x'
        matrix <- x$getmatrix()
        x.inv <- solve(matrix, ...)
        x$setinverse(x.inv)
        x.inv
}
        
