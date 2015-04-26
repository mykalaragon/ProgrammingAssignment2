## Calculate the inverse of a matrix only if it has not been calculated previously.

## Responsible for storing/returning the input matrix and inverse matrix.
makeCacheMatrix <- function(baseMatrix = matrix()) {
  inverseMatrix <- NULL
  
  setBaseMatrix <- function(inMatrix) {
    baseMatrix <<- inMatrix
    inverseMatrix <<- NULL
  }
  getBaseMatrix <- function() baseMatrix
  
  setInverseMatrix <- function(inverseValue) inverseMatrix <<- inverseValue
  getInverseMatrix <- function() inverseMatrix
  list(setBaseMatrix = setBaseMatrix, getBaseMatrix = getBaseMatrix,
       setInverseMatrix = setInverseMatrix,
       getInverseMatrix = getInverseMatrix)
}


## Conditionally calcualtes the inverse of a matrix and returns the calculation.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  result <- x$getInverseMatrix()
  
  if(!is.null(result)) {
    message("getting cached data")
    return(result)
  }
  data <- x$getBaseMatrix()
  result <- solve(data, ...)
  x$setInverseMatrix(result)
  result
}

#########################################
## Example on how to call these functions
######################################### 
## b <- makeCacheMatrix(B), where B is a square matrix
##
## b$getBaseMatrix(), returns the input matrix (B)
##
## b$setInverseMatrix(solve(B)), calculates the inverse matrix and stores it in b
## 
## b$getInverseMatrix(), returns the inverse matrix
##
## c <- cacheSolve(b), creates an obj and uses the incoming funcVar
##      (DOES NOT HOLD A REFERENCE, RATHER, JUST HOLDS THE LAST STATE OF b)

