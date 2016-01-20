## In the first part, I create a list of function to set the value of the matrix, get the value of the matrix, set the value of inverse, and get the value of inverse respectively


makeCacheMatrix <- function(x = matrix()) {
    inv<- NULL
    set<- function(y) {
        x<<- y
        inv<<- NULL
    }
    get<- function() x
    setinverse<- function(inverse) inv<<- inverse
    getinverse<- function() inv
    list(set = set, get = get, 
         setinverse = setinverse,
         getinverse = getinverse)

}


## The following function calculates the inverse of the special "matrix"
##  created by makeCacheMatrix above. If the inverse has already been 
##  calculated (which means the matrix has not changed), then it should
##  get the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## get a matrix that is the inverse of 'x'
    inv<- x$getinverse()
    if(!is.null(inv)){
            message("getting cached data")
            return(inv)
    }
    data <- x$get()
    inv<- solve(data, ...)
    x$setinverse(inv)
    inv
        ## Return a matrix that is the inverse of 'x'
}
