## The goal of this project is to store the inverse of a matrix in the cache and retrieve in the future to save computing time

## This function takes a matrix input and create a list of functions to read/set/store the information regarding its inverse

makeCacheMatrix <- function(x = matrix()) {
  mem <- NULL
  set <- function(y){
    x <<- y
    mem <<- NULL
  }
  
  get <- function() x
  setinv <- function(inv) mem <<- inv
  getinv <- function() mem
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## This function takes the return from makeCacheMatrix and retrieve or compute the inverse of the matrix
## The function is stopped and return NULL if the matrix is not invertible

cacheSolve <- function(x, ...) {
  mem <- x$getinv()
  data <- x$get()
  
  if(!is.null(mem)){
    message("getting cached data")
    return(mem)
  }
  
  if(det(data)==0){                          
    message("This matrix is not invertible")     
    return(NULL)
  }
  result <- solve(data, ...)
  x$setinv(result)
  
  return(result)
}
