
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
