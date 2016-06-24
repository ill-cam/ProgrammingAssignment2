## For any given matrix, the first function (makeCacheMatrix) caches 
## the value of the matrix as well as its inverse, and the second
## function (cacheSolve) either calculates the matrix's inverse and 
## prints its value or just prints the inverse's value if already calculated

## This function creates a list to cache the value of the
## matrix in the argument as well as the value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL ## Creates a local variable to ultimately store the inverse matrix
  set <- function(y) { ## Creates a function to cache the value of the matrix 
## in the argument
    x <<- y ## Special operator makes value of x available outside this function
    v <<- NULL ## Special operator makes value of v available outside this function
  }
  get <- function() x ## Creates a function to return the cached value of the matrix
  setinv <- function(inv) v <<- inv ## Creates function that updates the cached
## value of the inverse matrix
  getinv <- function() v ## Creates function to return the cached value of the
## inverse matrix
  list(set = set, get = get, setinv = setinv, getinv = getinv) ## Creates a list of
##functions to access from other functions in global environment
}


## The function below either calculates a matrix's inverse and 
## prints its value or just prints the inverse's value if already calculated

cacheSolve <- function(x, ...) {
  v <- x$getinv() ## Assigns cached inverse matrix value (may be null) to v
  if(!is.null(m)) {
    message("getting cached data")
    return(v) ## If there is a cached inverse matrix, returns the value and the
##function is complete
  } ## The rest of this code determines what happens if an inverse matrix value
##has not been cached
  data <- x$get() ## Accesses cached matrix value and assigns to new matrix ("data")
  v <- solve(data) ## Calculates the inverse matrix for "data"
  x$setinv(v) ## Caches the inverse matrix value
  v ## Prints the inverse matrix value
}