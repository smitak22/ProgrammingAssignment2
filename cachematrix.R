## File contains two functions:
## makeCacheMatrix: This function converts input matrix into 
##                  a special "matrix" object that can cache its inverse.
## cacheSolve:      This function checks if inverse of martix in question is 
##                  already cached, if not it computes the inverse and stores 
##                  into the cache.


## This function converts input matrix into a 
## special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inversed_m <- NULL
  
  ## function adds input matrix into cache and resets its inversed matrix
  set <- function(y){
    x <<- y
    inversed_m <<- NULL
  }
  
  ## funtion returns input matrix
  get <- function(){
    x
  }
  
  ## function returns the inversed version of current matrix from cache
  get_inversed_m <- function(){
    inversed_m
  }
  
  ## function adds the inversed version of current matrix into cache
  set_inversed_m <- function(m){
    inversed_m <<-m
  }
  
  list(set = set, get = get, get_inversed_m = 
         get_inversed_m, set_inversed_m = set_inversed_m)
  
}


## This function checks if inverse of martix in question is already
## cached, if not it computes the inverse and stores into the cache.

cacheSolve <- function(x, ...) {
  
  ## attempt to get inverse from cache
  output <- x$get_inversed_m()
  
  ## Check if inverse actually computed before
  if(!is.null(output)){
    message("Found cached data")
    return(output)
  }
  
  data <- x$get()
  output <- solve(data, ...)
  
  ## store inverse into cache
  x$set_inversed_m(output)
  output  
}
