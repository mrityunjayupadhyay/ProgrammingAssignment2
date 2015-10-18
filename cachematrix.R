## This script contains wrapper implementation of cached inverse matrix call

##usage/how to run
# 1. Create matrix
#     mat<-cbind(c(3,7,1),c(5,12,4),c(8,10,2))
#
# 2. Pass it to makeCacheMatrix to get wrapper object
#   myMat <- makeCacheMatrix(mat)
#
# 3. call caheSolve with wrapper object.
#   cacheSolve(myMat)
#   Frist call to this function will compute inverse and will not show message "getting cached data"
#   subseqent calls will return cached object ans show this message.



## This function provides wrapper on matrix which saves/computes inverse of matrix only once.

makeCacheMatrix <- function(x = matrix()) {

  #This is cached inverse, initilized to null.
  #will be calculate on first call of inverse.
  myMatrixInverse <- NULL
  
  #local function to calculate inverse of matrix
  inverse <- function(...){
    if(!is.null(x))
      myMatrixInverse <<- solve(x,...)
    else
      #Error message
      print("No Matrix is not provided to inverse!")
  }
  
  #Retrun cached inverse object. if its null we should call inverse to calculate it.
  getInverse <- function (){
    myMatrixInverse
  }
  

  #You can set diffrent matrix, this will reset cached inverse to NULL and it will be computed again on first call.
  setMatrix <- function(inputMatrix){
    x <<- inputMatrix
    myMatrixInverse<<- NULL
  }
  

  #List of functions to operate this wrapper.
  list(inverse = inverse, getInverse = getInverse, setMatrix = setMatrix)
  
}


#This function uses makeCacheMatrix's list object to return cached inverse.
#if cache is not yet calculated it will call inverse to do so, else it will return cached result.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  #get cached result.
  inverseMatrix <- x$getInverse()
  
  #if cached result is present.
  if(!is.null(inverseMatrix)){
    
    message("getting cached data")
    inverseMatrix
  }
  else{
        #compute inverse and cache it
        x$inverse()
        #return from cache
        x$getInverse()
  }
}

