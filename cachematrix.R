## Demo functions to illustrate caching the result of lengthy calculation
## makeCacheMatrix builds the feeder matrix and defines the active functions
## cacheSolve determines whether the solve has been previously cached or must be
## computed and then returns the value.
## R programming class
## 19Sep15


## feeder function, defining matrix manipulation functions

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL

    set <- function(y){
        x<<-y
        m<<-NULL
    }
    get <- function()x
    setinv <- function(solve) m <<- solve
    getinv <- function() m
    list(set=set, get=get, setinv=setinv,getinv=getinv)

}


## Demonstrate caching.  Check to see if solve exists in cache, if so return it
## otherwise, compute the solve and cache it.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ##check for cached solution
    m<-x$getinv()
    if(!is.null(m)){
        ## exists, return pre-calc'd values
        message("getting cached data...")
        return(m)
    }
    ## no cacehd solution, pull matrix data and solve
    data <- x$get()
    m <- solve(data,...)

    ## store calc'd value in cache
    x$setinv(m)
    m
}
