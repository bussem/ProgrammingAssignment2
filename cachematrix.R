## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a list of functions to set the value of a matrix,
## get, set the value of its inverse and calculate the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inver) inv <<- inver
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve checks if the inverse of the matrix is avail,
## if not, it will calcualte it and set all the right values. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  matrix <- x$get()
  inv <- solve(matrix,...)
  x$setinv(inv)
  inv
}


## to check code:
## testm <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow = TRUE)
## testmC <- makeCacheMatrix(testm)
## testmI <- cacheSolve(testmC)
## testm %*% testmI
## this should give the identity matrix