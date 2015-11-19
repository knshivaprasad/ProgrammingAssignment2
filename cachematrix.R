## This function creates a special matrix along with set of  functions that operates
## on the matrix. 
## Set function sets matrix
## get function retrives the matrix
## setInverseMatrix sets the inverse matrix
## getInverseMatrix gets the inverse matrix
## This function allows basic operations such as storing and retriving the matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x<<-y
    i<<-NULL
  }
  
  get <- function() x
  setInverseMatrix <- function(inverseMatrix) i<<-inverseMatrix
  getInverseMatrix <- function() i
  list (set=set,get=get,setInverseMatrix=setInverseMatrix,getInverseMatrix=getInverseMatrix)
}


## Using the operator functions makes sure that the matrix inverse is not recalculated 
## if it is in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invMatrix <- x$getInverse()
  if(!is.null(invMatrix))
  {
    message("Getting data from Cache..")
    return(invMatrix)
  }
  data<-x$get()
  invMatrix <- solve(data,...)
  x$setInverseMatrix(invMatrix)
  return(invMatrix)
  
}
