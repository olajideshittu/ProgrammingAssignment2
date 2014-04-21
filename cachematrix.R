## Script to caches the repeated matrix inversion operation performed on 
## same matrix. It stores the value returned by solve operation as a part
## of the matrix itself and gives the cached result if the inverse operation
## was performed before.


## Created the cache-matrix datastructure (list) that stores 
##      1. Stores matrix data
##      2. Has get() and set() function to get and set
##              matrix data
##      3. Has getInverse() and setInverse() functions
##              to get and set inverse of the stored matrix
## Argument : Input of class "matrix"
## Output   : list with function handle to get and set matrix 
##              and its inverse

makeCacheMatrix <- function(x = matrix()){
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<-NULL
        }
        get <- function(){
                x
        }
        setInverse <- function(solve){
                inv <<- solve
        }
        
        getInverse <- function() inv
        
        list(set = set, get = get, setInverse = setInverse, 
             getInverse = getInverse)
}


## Function to find inverse of a matrix. 
## Argument : matrix created using "makeCacheMatrix()" function
## Output   : Inverse of the matrix
cacheSolve <- function(x,...){
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        ## Checks if the inverse was calculted before.
        if(!is.null(inv)){
                ## Returns the cached result
                message("Getting cached data...")
                return(inv)
        }
        ## Retrieve the matrix from cached-matrix datastructure
        mat <- x$get()
        ## Calculate the inverse
        ## Will throw an error if the matrix in not a square matrix
        inv <- solve(mat)
        ## Saves the result
        x$setInverse(inv)
        inv
}