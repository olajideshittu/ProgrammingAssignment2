## creates object that store matrix and inversed matrix
makeCacheMatrix <- function(x = matrix()) {
  inversed <- NULL
  set <- function(y) {
    x <<- y
    inversed <<- NULL
  }
  get <- function() {
    x
  }
  setinversed <- function(inverse){
    inversed <<- inverse
  } 
  getinversed <- function() {
    inversed
  }
  list(
    set = set, 
    get = get,
    setinversed = setinversed,
    getinversed = getinversed
  )
}

## calculates inversed matrix of specific object and cheched it.
## if inversed matrix is already calculated return it from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inversed <- x$getinversed()
  if(!is.null(inversed)) {
    message("getting cached data")
    return(inversed)
  }
  data <- x$get()
  inversed <- solve(data, ...)
  x$setinversed(inversed)
  inversed
}
