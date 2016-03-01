makeCacheMatrix <- function(x = matrix()) {
  orig <- NULL # sets the value of orig to NULL
  inv <- NULL # sets the value of inv to NULL
  
  setmatrix <- function(y) { # set the value of the matrix
    orig <<- y  # caches the inputted matrix so that cacheSolve can check whether it has changed (note this is within the setmatrix function)
    inv <<- NULL # sets the value of inv (the matrix inverse if used cacheSolve) to NULL
  }
  getmatrix <- function(){
    orig
  }
  setinverse <- function(y){
    print("setinverse")
      inv <<- y
  }
  getinverse <- function(){
    print("getinverse")
    inv 
  }
  setmatrix(x) 
  list(setmatrix = setmatrix, getmatrix = getmatrix,  # creates a list to house the four functions
       setinverse = setinverse,
       getinverse = getinverse)

}


cacheSolve <- function (x=matrix(), ...) {
  m <- x$getinverse() # if an inverse has already been calculated this gets it
  if(!is.null(m)){ # check to see if cacheSolve has been run before
    if(x$setmatrix() == x$getmatrix()) { # check that matrix hasn't changed, and if it hasn't, sends a text message and returns the cached matrix

      return(m) 
    }
  }
    y <- x$getmatrix() # run the getmatrix function to get the value of the input matrix
    m <- solve(y, ...) # compute the value of the inverse of the input matrix
    x$setinverse(m) # run the setinverse function on the inverse to cache the inverse
    m # return the inverse
}