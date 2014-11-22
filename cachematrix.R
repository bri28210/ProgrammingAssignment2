## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix : is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly
## This function creates a special "matrix" object that can cache its inverse.
## 1- set the value of the matrix
## 2- get the value of the matrix
## 3- set the value of the mean
## 4- get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
    inver <- NULL
    set <- function(y) {
      x <<- y
      inver <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inver <<- inverse
    getinverse <- function() inver
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}



## cacheSolve :
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  invers <- x$getinverse()
  if(!is.null(invers)) {
    message("getting cached data")
    return(invers)
  }
  data <- x$get()
  invers <- inverse(data, ...)
  x$setinverse(invers)
  invers
        ## Return a matrix that is the inverse of 'x'
}

