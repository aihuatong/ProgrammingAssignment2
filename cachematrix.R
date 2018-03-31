## This R code file includes two R functions that are able to cache inverse computation of a matrix

## Function makeCacheMatrix accepts a matrix as argument, 
## creates a special "vector", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  inv_m <- NULL
  set <- function(y) {
    x <<- y
    inv_m <<- NULL
  }
  get <- function() x
  setSolve <- function(inv) inv_m <<- inv
  getSolve <- function() inv_m
  list(set = set, get = get,
       setSolve = setSolve,
       getSolve = getSolve)
}


## Function cacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix function
## if the inverse of 'x' has already exists and the matrix has not changed, return the cached inverse of 'X'
## otherwise, calculate, cache  and return the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv_m <- x$getSolve()
  if(!is.null(inv_m)) {
    message("getting cached data")
    return(inv_m)
  }
  data <- x$get()
  inv_m <- solve(data, ...)
  x$setSolve(inv_m)
  inv_m
}

## testing
## m1 <- matrix(c(1/2, -1/4, -1, 3/4), nrow = 2, ncol = 2)
## m1
## [,1]  [,2]
## [1,]  0.50 -1.00
## [2,] -0.25  0.75
## cacheSolve(makeCacheMatrix(m1))
## [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## > cacheSolve(makeCacheMatrix(m1))
## [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## > cacheSolve(myMatrix_object)
## [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## > cacheSolve(myMatrix_object)
## getting cached data
## [,1] [,2]
## [1,]    6    8
## [2,]    2    4
## n2 <- matrix(c(5/8, -1/8, -7/8, 3/8), nrow = 2, ncol = 2)
## myMatrix_object$set(n2)
## > cacheSolve(myMatrix_object)
## [,1] [,2]
## [1,]    3    7
## [2,]    1    5
## > cacheSolve(myMatrix_object)
## getting cached data
## [,1] [,2]
## [1,]    3    7
## [2,]    1    5
## myMatrix_object$get()
## [,1]   [,2]
## [1,]  0.625 -0.875
## [2,] -0.125  0.375
## > n2
## [,1]   [,2]
## [1,]  0.625 -0.875
## [2,] -0.125  0.375