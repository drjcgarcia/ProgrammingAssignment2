## Assignment: Caching the Inverse of a Matrix
## Author:  Juan C. Garcia
## Date: 01-Jan-2022
## Version: 1

## The first function, 'makeCacheMatrix', creates a special "matrix" object
## that can cache its inverse.
##   1.  set the value of the matrix
##   2.  get the value of the matrix
##   3.  set the inverse of the matrix
##   4.  get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The second function, 'cacheSolve', computes the inverse of the special
## "matrix" returned by `makeCacheMatrix`.
##   1. Check to see if the inverse has already been calculated. 
##   2. If so, `get`s the inverse from the cache and skips computation. 
##   3. Otherwise, calculates the inverse, using solve() function, of the matrix 
##      and set value via 'setinverse' function.

cacheinverse <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}