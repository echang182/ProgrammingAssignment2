## Put comments here that give an overall description of what your
## functions do

## given a Matrix x, this function create a list of functions that 
## work with the matrix and its inverse in cache memory 

makeCacheMatrix <- function(x = matrix()) {
  INV <- NULL
  set <- function(y) {
    x <<- y
    INV <<- NULL
  }
  get <- function() x
  setinv <- function(solve) INV <<- solve
  getinv <- function() INV
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## given the list of functions x, this function look if there is 
## the inverse of the matrix x (from the function makeCacheMatrix) 
## saved in cache, if is it, then return it, else, calculate it 
## and store it in cache memory

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  INV <- x$getinv()
  if(!is.null(INV)) {
    message("getting cached data")
    return(INV)
  }
  data <- x$get()
  INV <- solve(data, ...)
  x$setinv(INV)
  INV
}
