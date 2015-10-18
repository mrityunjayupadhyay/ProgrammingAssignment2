## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

  myMatrixInverse <- NULL
  
  inverse <- function(...){
    if(!is.null(x))
      myMatrixInverse <<- solve(x,...)
    else
      print("No Matrix is not provided to inverse!")
  }
  
  getInverse <- function (){
    myMatrixInverse
  }
  
  setMatrix <- function(inputMatrix){
    x <<- inputMatrix
    myMatrixInverse<<- NULL
  }
  
  list(inverse = inverse, getInverse = getInverse, setMatrix = setMatrix)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  
  inverseMatrix <- x$getInverse()
  
  if(!is.null(inverseMatrix)){
    message("getting cached data")
    inverseMatrix
  }
  else{
    x$inverse()
    x$getInverse()
  }
}
