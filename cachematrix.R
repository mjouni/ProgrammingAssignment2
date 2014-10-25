## This R script will expose functions that allow a caller to compute
## the inverse of a matrix while speeding up the calculation by pulling
## previously computed values from the cache. 

## Takes an invertible matrix as input and exposes
## a set of functions that enables the caller to 
## cache the inverse of a matrix
## Input: An invertible matrix
## Output: A special vector that contains the following functions
##     set: stores the supplied matrix and resets the previously computed inverse
##     get: returns the matrix 
##     setinverse: takes as parameter the inverse matrix to be stored in cache
##     getinverse: Returns the stored inverse matrix
makeCacheMatrix <- function(m = matrix()) 
{
  i <- NULL
  set <- function(y) {
    m <<- y
    i <<- NULL
  }
  get <- function() m
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Takes a special matrix vector and returns its inverse. If the inverse has already
## been computed - and the matrix has not changed - the inverse is retrieved from cache
## Input : A special matrix vector created from a makeCacheMatrix call
## Output: Inverse of the matrix. 
cacheSolve <- function(m, ...)
{
  i <- m$getinverse()
  
  if (!is.null(i))
  {
    message("Printing cached inverse")
    return(i) 
  }
  
  data <- m$get()
  i <- solve(data, ...)
  m$setinverse(i)
  i
  
}