makeCacheMatrix <- function(x = matrix()) {
  orig <- NULL 
  inv <- NULL 
  
  setmatrix <- function(y) { 
    orig <<- y 
    inv <<- NULL 
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
  list(setmatrix = setmatrix, getmatrix = getmatrix, 
       setinverse = setinverse,
       getinverse = getinverse)
  
}


cacheSolve <- function (x=matrix(), ...) {
  m <- x$getinverse() 
  if(!is.null(m)){ 
    if(x$setmatrix() == x$getmatrix()) { 
      return(m)
    }
  }
  y <- x$getmatrix() 
  m <- solve(y, ...) 
  x$setinverse(m) 
  m 
}