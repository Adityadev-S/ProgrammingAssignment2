## 'makeCacheMatrix' and 'cacheSolve' functions are written to first calculate 
## the inverse of a matrix and then to cache the inverse so that it can be
## used for future calculations without time loss.

## The first function 'makeCacheMatrix' is a list of 4 functions - set , get,
## setmatrix and getinverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  } ## function used to set the value of the entered matrix
  get <- function() {
    x
  } ## function used to get the value of entered matrix into cacheMean function 
  setmatrix <- function(solve) {
    m <<- solve 
  } ## function used to set the output m = (inverse matrix)     
  getinverse <- function(){
    m
  } ## function used to fetch the inverse of the entered matrix
  
  list(set = set, get = get,
       setmatrix = setmatrix,
       getinverse = getinverse)
  
}


## cacheSolve is the function used to calculate the inverse of the entered matrix

cacheSolve <- function(x,... ) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  } ## if condition is used to check if the inverse has already been cached
  data <- x$get()
  m <- solve(data, ...) ## since we are passing only one matrix - 'data', second matrix passed is the identity matrix in the solve function. Output - inverse of 'data'.
  x$setmatrix(m)
  m
}