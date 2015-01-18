## Put comments here that give an overall description of what your
## functions do

## This function takes as an argument a matrix that's invertible
## The function 

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL
    
    ## This is a definition of a sub function called "set"
    ## "set" takes y as an argument and stores that value to x
    ## "set" also sets the inv variable to null
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## This is a definition of a sub function called "set"
    ## "get" simply returns x
    get <- function() x

    ## This is a definition of a sub function called "setinverse"
    ## "setinverse" takes a calculated value (the inverse) and caches
    ## it in the variable inv
    setinverse <- function(inverse) inv <<-inverse
    
    ## This is a definition of a sub function called "getinverse"
    ## "get" simply returns the inverse value "inv"
    getinverse <- function() inv
    
    ## This code creates the list that is returned
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse
         )
}


## This function takes a function as its argument, specifically
## the makeCacheMatrix function.  This function first checks its
## argument function to see if the inverse has already been calculated
## and stored.  If it has, it uses that value.  If it hasn't, it calculates
## the inverse and stores it for future use.  Then if finally returns
## the inverse value.

cacheSolve <- function(x, ...) {
    
    ## get the inverse from the makeCacheMatrix function        
    inv <- x$getinverse()
    
    ## if something is stored, return that value
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## if nothing was stored, get the matrix and do the calculation
    data <- x$get()
    inv <- solve(data,...)
    x$setinverse(inv)
    inv
}
