# cachematrix.R
#
# Author    : Sean P Murphy
# Date      : 01-15-2016
# 
# Programming Assignment 2: Caching the Inverse of a Matrix
# R Programming Course
#
################################################################################
# How to test this file:
#
#  1. Create a matrix object:
#   matx <- matrix(c(1,2,4,4),2,2)
#
#  2. Pass the matrix parameter to the makeCacheMatrix() function:
#   make <- makeCacheMatrix(matx)
#
#  3. Pass this object to cacheSolve() function:
#   cacheSolve(make)

# Example run:

# > source('C:/Users/zodiac/ProgrammingAssignment2/cachematrix.R')
# > matx <- matrix(c(1,2,4,4),2,2)
# > matx
# [,1] [,2]
# [1,]    1    4
# [2,]    2    4
# > make <- makeCacheMatrix(matx)
# > cacheSolve(make)
# [,1]  [,2]
# [1,] -1.0  1.00
# [2,]  0.5 -0.25
#
################################################################################

# makeCacheMatrix() makes a matrix object and caches its inverse.
# the list sets the value, gets the value, sets inverse, gets inverse
makeCacheMatrix <- function(x = matrix()) {
  n <- NULL
  set <- function(y) {
    x <<- y
    n <<- NULL
  }
  get <- function() x
  setinvers <- function(solve) n <<- solve
  getinvers <- function() n
  list(set = set, get = get,
       setinvers = setinvers,
       getinvers = getinvers)
}

# cacheSolve() finds inverse matrix returned by makeCacheMatrix()
# if inverse is set, finds inverse from cache

cacheSolve <- function(x, ...) {
  n <- x$getinvers()
  if(!is.null(n)) {
    return(n)
  }
  data <- x$get()
  n <- solve(data, ...)
  x$setinvers(n)
  n
}
