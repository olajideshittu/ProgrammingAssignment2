## Two functions that are used to create a special object
## that stores a matrix and cache's its inverse
## 

## Function: makeCacheMatrix
## Description: This function creates a special "matrix" object 
## 				that can cache it inverse

makeCacheMatrix <- function(matrix = matrix()) {
	#invMatrix - inverse matrix value
	invMatrix <- NULL #default value - NULL

	#setMatrix function: matrix and reset invMatrix
	setMatrix <- function(y){
		matrix <<- y
		invMatrix <<- NULL
	}
	#getMatrix function: get matrix
	getMatrix <- function() matrix

	#setInvMatrix function: set inverse matrix
	setInvMatrix <- function(inverseMatrix) invMatrix <<- inverseMatrix

	#getInvMatrix function: get inverse matrix
	getInvMatrix <- function() invMatrix
	
	list(setMatrix = setMatrix, getMatrix=getMatrix,
		 setInvMatrix = setInvMatrix,
		 getInvMatrix = getInvMatrix)
}


## Function: cacheSolve
## Description: This function computes the inverse of the
## 				special "matrix" returned by makeCacheMatrix

cacheSolve <- function(matrix, ...) {
		#get invMatrix
        invMatrix <- matrix$getInvMatrix()

        #check if it is not null
        if (!is.null(invMatrix)){
        	message("getting cached data")
        	return (invMatrix)
        }

        #if it is null, calculate the inverse
        data <- matrix$getMatrix()
        inv <- solve(data, ...)
        matrix$setInvMatrix(inv) #save it
        inv
}
