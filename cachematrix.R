## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  # set function to update the matrix and reset the cached inverse
  set <- function(y) {
    x <<- y     # update x in the parent environment
    m <<- NULL  # reset m to NULL since the matrix has changed
  }
  
  # get function to return the current matrix
  get <- function() x
  
  # setinverse function to store the inverse matrix in m
  setinverse <- function(solve) m <<- solve
  
  # getinverse function to return the cached inverse matrix
  getinverse <- function() m
  
  # return a list of functions to interact with the cached matrix and its inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
  
  # if the inverse is already cached, return it with a message
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  # otherwise, get the matrix, calculate its inverse, cache it, and return the inverse
  data <- x$get()      # retrieve the matrix
  m <- solve(data, ...) # calculate the inverse
  x$setinverse(m)      # cache the inverse
  m              
}
