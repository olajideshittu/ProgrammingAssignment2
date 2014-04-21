# The makeCacheMatrix function and the cacheSolve function below leverage R's 
# lexical scoping rules together with the  <<- assignemnt operator to implement
# a solution for caching the inverse of square invertible matrices.


makeCacheMatrix <- function(x = matrix()) {
  # Creates a special "matrix" object that can cache its inverse and which 
  # contains 3 functions (get, setinverse, getinverse) to access and manipulate
  # its attributes.
  #
  # Args:
  #   x: A square invertible matrix 
  # 
  # Returns:
  #   The square invertible matrix bind to a new environment including 3 nested
  #   functions.
  inv <- NULL
  get <- function() x
  # The <<- operator is used to assign a value to a variable in the parent 
  # enviroment. 
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  return(list(get = get, 
              setinverse = setinverse,
              getinverse = getinverse))
}

 
cacheSolve <- function(x, ...) {
  # Computes the inverse of a special "matrix" object. If the inverse has 
  # already been calculated, then the inverse is retrieved from the cache.
  #
  # Args: 
  #   x: A special "matrix" object returned by makeCacheMatrix()
  #
  # Returns:
  #   The inverse of a special "matrix" object
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
}