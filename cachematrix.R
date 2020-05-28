# Programming Assignment 2: Lexical Scoping

## Function 1: makeCacheMatrix creates a matrix object
## that can cache the inverse of the matrix. 

## Function 2: cacheSolve computes the inverse of the cached matrix
## from makeCacheMatrix


## Function 1
## makeCacheMatrix stores the inverse of a given matrix, x. 
## set() is used to set the value of the matrix, x and its inverse, inv 
## so that they may take different values from the parent environment.
## The argument inv <<- NULL clears inv of any prior cached value.
## get() is used to get the value of the matrix, setinv sets the value of
## the inverse, and getinv gets the value of the inverse.
## list() is used to set each argument as an element in a list

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
}
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## Function 2
## cacheSolve retrieves the inverse from the object/matrix passed 
## to the makeCacheMatrix argument. This function checks to see if the 
## value of the cached inverse is set and if it is, it returns the message
## "getting cached data" followed by the value of the inverse.
## If the cache is empty, the function retrieves the matrix from 
## the inputed data and solves and prints the inverse


cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
