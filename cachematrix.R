## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

    xinv<- NULL
    set<-function(y) {
        x<<-matrix(y)
        xinv<<- NULL
    }
    get <- function() x
    setinv<- function(matinv) xinv<<-matinv
    getinv<- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    xinv <- x$getinv()
    if (!is.null(xinv)) {
        message("Getting cached data ...")
        return(xinv)
    }
    data <-x$get()
    xinv<-solve(data)
    x$setinv(xinv)
    xinv
}
