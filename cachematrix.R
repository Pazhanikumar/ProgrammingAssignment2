## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
##x is a square invertibel matrix
  ## function to get,set the matrix
  ##initializing the inverse
  
  inv = NULL
  set = function(y) {
    
    x <<- y
    inv <<- NULL
  }
  get = function() x
  setinverse = function(inverse) inv <<- inverse 
  getinverse = function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
##x is the output of makeCacheMatrix
  
  inv = x$getinverse()
  
  # if the inverse is calculated
  if (!is.null(inv)){
    # get that from the cache 
    message("getting cached data")
    return(inv)
  }
  
  # if not calculates the inverse 
  mat.data = x$get()
  inv = solve(mat.data, ...)
  
  # sets the inverse
  x$setinverse(inv)
  
  return(inv)
        ## Return a matrix that is the inverse of 'x'
}
