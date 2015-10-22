#-----------------------------------------
#  Dean Tolla
#  10/15/15
#  R-Programming
#  Programming Assignment 2, Class Week 3
# 
#  Create functions that allow the inverse of a 
#   matrix to be cached and retrieved to save 
#   on computational resources
#-----------------------------------------


#  Create a "matrix", which is actually a list 
#   containing a function to do the following
#  1. set the value of the matrix
#  2. get the value of the matrix
#  3. set the value of the inverse
#  4. get the value of the inverse

makeCacheMatrix = function(A = matrix()) {
  
  # invA will hold the inverse of matrix A
  invA <- NULL
  
  # assign A the value of a matrix B from the calling environment
  set <- function(B) {
    A <<- B
    
    # sets the inverse to back to NULL if the value of A is changed
    invA <<- NULL
  }
  
  # return the value held in matrix A
  get <- function() A
  
  # set the value of invA from the calling environment
  setInverse <- function(inverse) {
    invA <<- inverse
  }
  
  # return the value of invA
  getInverse <- function() invA
  
  # the list returned by makeCacheMatrix()
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


#  Calculate the inverse of the special "matrix" if needed 
#   or skip the calculation and return the cached inverse

cacheSolve <- function(A, ...) {
  
  # getting the value (if any) of the inverse
  invA <- A$getInverse()
  
  # checking if the inverse is cached
  if(!is.null(invA)) {
    message("getting cached data")
    
    # if the inverse is cached return it and exit this function
    return(invA) 
  }
  
  # if the inverse is not cached, calculate it and cache it
  data <- A$get()
  invA <- solve(data, ...)
  A$setInverse(invA)
  invA
}