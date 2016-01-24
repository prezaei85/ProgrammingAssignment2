## make a special "matrix" object that stores a matrix and 
## caches its inverse

## First, make a special "matrix" object that stores a matrix 
## and the functions needed to get the metrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
  x_inv <- NULL
  set <- function(A) {
    x <<- A
    x_inv <<- NULL 
  }
  get <- function() x
  set_inv <- function(B) x_inv <<- B
  get_inv <- function() x_inv
  list(set = set, get = get,
       set_inv = set_inv,
       get_inv = get_inv)
}  

## Check the cach to see if the inverse is already computed 
## and stored, otherwise, compute the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  x_inv <- x$get_inv()
  if (!is.null(x_inv)) {
    message("getting cached data")
    return(x_inv)
  }
  this_x <- x$get()
  x_inv <- solve(this_x, ...)
  x$set_inv(x_inv)
  x_inv
}
