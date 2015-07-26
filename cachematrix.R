## makeCacheMatrix`: This function creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  #Same structure of a vector but with a matrix object
  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(inverser) Inv <<- inverse
  getInv <- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## cacheSolve: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above. If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.
##

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
  if(!is.null(Inv)) {
    message("getting cached data")
    return(Inv)
    }    
  data <- x$get()
  Inv <- solve(data, ...)
  x$setInv(Inv)
  Inv
  
  
}
