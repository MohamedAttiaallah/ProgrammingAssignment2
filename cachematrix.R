## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## The following functions makeCacheMatrix() and cacheSolve() work together to return the inverse 
## of a matrix that was passed to it. 

## makeCacheMatrix creates a R object that stores the matrix thats been passed to it (defined as x)
##as well as the inverse of the matrix (defined as i) given by cacheSolve() function. 
## It also contains other useful functions that it passes to the R object as a list. 
##These functions are set() which is called to change the stored matrix X, get() which is called 
##to retrieve the stored matrix x, setinverse() which is called by cacheSolve() to store the 
##inverse matrix that it has calculated and getinverse(), which is called to retrieve the stored inverse matrix.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}



## Write a short comment describing this function
## CacheSolve() receives a R object of type MakeCacheMatrix, and checks if there's 
##already a matrix inverse stored, if there is, it will simply return this inverse matrix and stop.
##Otherwise, it will retrieve the stored matrix of the R object using the x$get() and apply the solve() 
##function to work out the inverse and cache( i.e. store ) the result in the R object using x$setinverse()

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if (!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}