## Put comments here that give an overall description of what your
## functions do

## This function creates a new matrix object that has a list of
## functions to get and set the inverse of a matrix
makeCacheMatrix <- function(x = matrix()) {
  #Set the value to null so we can check to see if it has been set
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # return the value of the matrix
  get <- function() x
  
  # set the value of the matrix inverse
  setinverse <- function(sovle) inv <<- solve
  
  # get the value of the matrix inverse
  getinverse <- function() inv
  
  # describe the functions available
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  
  #get the inverse of the matrix
  inv <- x$getinverse()
  
  #check to see if it is calculated before
  if(!is.null(inv)) {
    print("getting cached data")
    return(inv)
  }
  
  #we need to calculate the inverse
  data <- x$get()
  inv <- solve(data, ...)
  
  #set the inverse for later
  x$setinverse(inv)
  
  
  return(inv)
}
