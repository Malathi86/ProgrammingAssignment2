## Functions below create a special object that stores a matrix, and cache it's inverse.
## These functions help in reducing the computational time and cost of calculating inverse of a matrix.

## "makeCacheMatrix" function creates a special "matrix" object; a list containing functions
## to set & get the value of a matrix, set the inverse of the matrix, and get the cached inverse of the matrix.
## input -> An invertible matrix

makeCacheMatrix <- function(i = matrix()) {
    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() {
        x
    }
    setinverse <- function(inv){
        i <<- inv
    } 
    getinverse <- function(){
        i
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function calculates the inverse of a matrix created with function "makeCacheMatrix"
## If the inverse has already been calculated and the matrix is same, then this function retrieves the inverse from cache. 
## Otherwise, inverse is calculated and cached.

cacheSolve <- function(x, ...) {
    
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message(">>> Retrieving data from cache")
        return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
