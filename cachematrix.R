## For any given matrix, the first function (makeCacheMatrix) caches 
## the value of the matrix as well as its inverse, and the second
## function (cacheSolve) either calculates the matrix's inverse and 
## prints its value or just prints the inverse's value if already calculated

## This function creates a list to cache the value of the
## matrix in the argument as well as the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setinv <- function(inv) v <<- inv
  getinv <- function() v
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## The function below either calculates a matrix's inverse and 
## prints its value or just prints the inverse's value if already calculated

cacheSolve <- function(x, ...) {
  v <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data)
  x$setinv(v)
  v
}