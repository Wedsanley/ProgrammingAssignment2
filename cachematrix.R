## Cache the inverse of a matrix, prevent computing it repeatedly

# function that creates a special matrix able to cache its inverse
# the final list contains function to set the value of the matrix,
# get the value of the matrix
# set the value of the inverse and another one to get it.

makeCachematrix<-function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inverse <<- solve
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}  
  
# the cacheSolve function calculates the inverse of the special matrix
# created by the makeCachematrix function
# it checks if the inverse has been calculated, if yes it gets it from the cache
# and skip the computation.If not, it calculates the inverse, sets it in the
# cache via the setinverse function
  
  cacheSolve <- function(x, ...) {
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
      message("getting cached data")
      return(inverse)
    }
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse 
}